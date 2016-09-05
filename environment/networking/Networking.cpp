#include "Networking.hpp"

#include <fstream>
#include <sstream>
#include <algorithm>
#include <stdio.h>
#include <chrono>
#include <thread>
#include <mutex>

std::mutex coutMutex;

std::string serializeMapSize(const hlt::Map & map) {
    std::string returnString = "";
    std::ostringstream oss;
    oss << map.map_width << ' ' << map.map_height << ' ';
    returnString = oss.str();
    return returnString;
}

std::string serializeProductions(const hlt::Map & map) {
    std::string returnString = "";
    std::ostringstream oss;
    for(auto a = map.contents.begin(); a != map.contents.end(); a++) {
        for(auto b = a->begin(); b != a->end(); b++) {
            oss << (unsigned short)(b->production) << ' ';
        }
    }
    returnString = oss.str();
    return returnString;
}

std::string Networking::serializeMap(const hlt::Map & map) {
    std::string returnString = "";
    std::ostringstream oss;

    //Run-length encode of owners
    unsigned short currentOwner = map.contents[0][0].owner;
    unsigned short counter = 0;
    for(int a = 0; a < map.contents.size(); ++a) {
        for(int b = 0; b < map.contents[a].size(); ++b) {
            if(map.contents[a][b].owner == currentOwner) {
                counter++;
            }
            else {
                oss << (unsigned short)counter << ' ' << (unsigned short)currentOwner << ' ';
                counter = 1;
                currentOwner = map.contents[a][b].owner;
            }
        }
    }
    //Place the last run into the string
    oss << (unsigned short)counter << ' ' << (unsigned short)currentOwner << ' ';

    //Encoding of ages
    for(int a = 0; a < map.contents.size(); ++a) {
        for(int b = 0; b < map.contents[a].size(); ++b) {
            oss << (unsigned short)map.contents[a][b].strength << ' ';
        }
    }

    returnString = oss.str();

    return returnString;
}

std::map<hlt::Location, unsigned char> Networking::deserializeMoveSet(std::string & inputString, const hlt::Map & m) {
    std::map<hlt::Location, unsigned char> moves = std::map<hlt::Location, unsigned char>();

    if(std::find_if(inputString.begin(), inputString.end(), [](const char & c) -> bool { return (c < '0' || c > '9') && c != ' '; }) != inputString.end()) {
        if(!quiet_output) {
            std::string errorMessage = "Bot sent an invalid character - ejecting from game.\n";

            std::lock_guard<std::mutex> guard(coutMutex);
            std::cout << errorMessage;
        }
        throw inputString;
    }

    std::stringstream iss(inputString);
    hlt::Location l;
    int d;
    while (iss >> l.x >> l.y >> d && m.inBounds(l)) moves[l] = d;

    return moves;
}

void Networking::sendString(unsigned char playerTag, std::string sendString) {
    //End message with newline character
    sendString += '\n';

#ifdef _WIN32
    WinConnection connection = connections[playerTag - 1];

    DWORD charsWritten;
    bool success;
    success = WriteFile(connection.write, sendString.c_str(), sendString.length(), &charsWritten, NULL);
    if(!success || charsWritten == 0) {
        if(!quiet_output) std::cout << "Problem writing to pipe\n";
        throw 1;
    }
#else
    int connection = connections[playerTag - 1];
    do {
        int result = write(connection, sendString.c_str(), sendString.length());
        if(result < 0) {
            if(!quiet_output) std::cout << "Problem writing to pipe\n";
            throw 1;
        }
        sendString = sendString.substr(result);
    } while(sendString.length() > 0);
#endif
}

std::string Networking::getString(unsigned char playerTag, unsigned int timeoutMillis) {

    const int BUF_SIZE = 16384;
    char * buffer = new char[BUF_SIZE]; //Buffer as large as (I think) could possibly be necessary.
    std::string newString; //Our result.

    std::chrono::high_resolution_clock::time_point tp = std::chrono::high_resolution_clock::now();

#ifdef _WIN32

    WSAPOLLFD fd;
    fd.fd = connections[playerTag - 1];
    fd.events = POLLRDNORM;

    //Keep reading until a newline
    while(true) {

        //Check if there are bytes in the pipe
        timeoutMillis -= std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now() - tp).count();
        tp = std::chrono::high_resolution_clock::now();
        if(timeoutMillis < 0) {
            if(!quiet_output) {
                std::string errorMessage = "Bot #" + std::to_string(playerTag) + " timed out.\n";

                std::lock_guard<std::mutex> guard(coutMutex);
                std::cout << errorMessage;
            }
            throw newString;
        }
        int pollResult = WSAPoll(&fd, 1, 10); //10 millis.
        if(pollResult == SOCKET_ERROR) {
            if(!quiet_output) {
                std::string errorMessage = "Bot #" + std::to_string(playerTag) + " error.\n";

                std::lock_guard<std::mutex> guard(coutMutex);
                std::cout << errorMessage;
            }
            throw newString;
        }
        else if(pollResult > 0) {
            int messageSize = WSARecv(connections[playerTag - 1], buffer.data(), buffer.size(), 0);
            if(messageSize == SOCKET_ERROR /*Error*/ || messageSize == 0 /*Connection broken*/) {
                if(!quiet_output) {
                    std::string errorMessage = "Bot #" + std::to_string(playerTag) + " error.\n";

                    std::lock_guard<std::mutex> guard(coutMutex);
                    std::cout << errorMessage;
                }
                throw newString;
            }
            if(messageSize > 0) {
                int firstNewline = buffer.find_first_of('\n');
                if(firstNewline == std::string::npos || firstNewline >= messageSize) newString += buffer.substr(0, messageSize);
                else {
                    newString += buffer.substr(0, firstNewline);
                    break;
                }
            }
        }
    }
#else

    struct pollfd fd;
    fd.fd = connections[playerTag - 1];
    fd.events = POLLIN;

    //Keep reading until a newline
    while(true) {

        //Check if there are bytes in the pipe
        timeoutMillis -= std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now() - tp).count();
        tp = std::chrono::high_resolution_clock::now();
        if(timeoutMillis < 0) {
            if(!quiet_output) {
                std::string errorMessage = "Bot #" + std::to_string(playerTag) + " timed out.\n";
                std::cout << errorMessage;
            }
            throw newString;
        }
        int pollResult = poll(&fd, 1, 10); //10 millis.
        if(pollResult == -1) {
            if(!quiet_output) {
                std::string errorMessage = "Bot #" + std::to_string(playerTag) + " error.\n";

                std::lock_guard<std::mutex> guard(coutMutex);
                std::cout << errorMessage;
            }
            throw newString;
        }
        else if(pollResult != 0) {
            int messageSize = recv(connections[playerTag - 1], buffer, BUF_SIZE, 0);
            if(messageSize <= 0 /*<0 indicates error, 0 indicates not connected; both are bad*/) {
                if(!quiet_output) {
                    std::string errorMessage = "Bot #" + std::to_string(playerTag) + " error.\n";

                    std::lock_guard<std::mutex> guard(coutMutex);
                    std::cout << errorMessage;
                }
                throw newString;
            }
            if(messageSize > 0) {
                std::string bufStr(buffer);
                int firstNewline = bufStr.find_first_of('\n');
                if(firstNewline == std::string::npos || firstNewline >= messageSize) newString += bufStr.substr(0, messageSize);
                else {
                    newString += bufStr.substr(0, firstNewline);
                    break;
                }
            }
        }
    }
#endif
    //Python turns \n into \r\n
    if(newString.back() == '\r') newString.pop_back();

    return newString;
}

void Networking::startAndConnectBot(std::string command, int port) {
#ifdef _WIN32
    command = "/C " + command;

    WinConnection parentConnection, childConnection;

    SECURITY_ATTRIBUTES saAttr;
    saAttr.nLength = sizeof(SECURITY_ATTRIBUTES);
    saAttr.bInheritHandle = TRUE;
    saAttr.lpSecurityDescriptor = NULL;

    //Child stdout pipe
    if(!CreatePipe(&parentConnection.read, &childConnection.write, &saAttr, 0)) {
        if(!quiet_output) std::cout << "Could not create pipe\n";
        throw 1;
    }
    if(!SetHandleInformation(parentConnection.read, HANDLE_FLAG_INHERIT, 0)) throw 1;

    //Child stdin pipe
    if(!CreatePipe(&childConnection.read, &parentConnection.write, &saAttr, 0)) {
        if(!quiet_output) std::cout << "Could not create pipe\n";
        throw 1;
    }
    if(!SetHandleInformation(parentConnection.write, HANDLE_FLAG_INHERIT, 0)) throw 1;

    //MAKE SURE THIS MEMORY IS ERASED
    PROCESS_INFORMATION piProcInfo;
    ZeroMemory(&piProcInfo, sizeof(PROCESS_INFORMATION));

    STARTUPINFO siStartInfo;
    ZeroMemory(&siStartInfo, sizeof(STARTUPINFO));
    siStartInfo.cb = sizeof(STARTUPINFO);
    siStartInfo.hStdError = childConnection.write;
    siStartInfo.hStdOutput = childConnection.write;
    siStartInfo.hStdInput = childConnection.read;
    siStartInfo.dwFlags |= STARTF_USESTDHANDLES;

    //C:/xampp/htdocs/Halite/Halite/Debug/ExampleBot.exe
    //C:/Users/Michael/Anaconda3/python.exe
    //C:/Program Files/Java/jre7/bin/java.exe -cp C:/xampp/htdocs/Halite/AIResources/Java MyBot
    bool success = CreateProcess(
        "C:\\windows\\system32\\cmd.exe",
        LPSTR(command.c_str()),     //command line
        NULL,          //process security attributes
        NULL,          //primary thread security attributes
        TRUE,          //handles are inherited
        0,             //creation flags
        NULL,          //use parent's environment
        NULL,          //use parent's current directory
        &siStartInfo,  //STARTUPINFO pointer
        &piProcInfo
    );  //receives PROCESS_INFORMATION
    if(!success) {
        if(!quiet_output) std::cout << "Could not start process\n";
        throw 1;
    }
    else {
        CloseHandle(piProcInfo.hProcess);
        CloseHandle(piProcInfo.hThread);

        processes.push_back(piProcInfo.hProcess);
        connections.push_back(parentConnection);
    }
#else
    if(!quiet_output) std::cout << command << "\n";

    //Create socket.
    int sock = socket(AF_INET, SOCK_STREAM, 0);
    assert(sock >= 0);
    //Setup socket address.
    struct sockaddr_in serv_addr, clien_addr;
    unsigned int clien_addr_len = sizeof(clien_addr);
    bzero((char *) &serv_addr, sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_addr.s_addr = INADDR_ANY;
    serv_addr.sin_port = htons(port);
    int adr_result = bind(sock, (struct sockaddr *) &serv_addr, sizeof(serv_addr));
    assert(adr_result >= 0);
    //Set socket to non-blocking mode.
    int arg = fcntl(sock, F_GETFL, NULL);
    assert(arg >= 0);
    arg |= O_NONBLOCK;
    int bloc_result = fcntl(sock, F_SETFL, arg);
    assert(bloc_result >= 0);
    //Fork child process.
    int pid = fork();
    if(pid == 0) { //This is the child
        setpgid(getpid(), getpid()); //Set pid group.

        execl("/bin/sh", "sh", "-c", command.c_str(), (char*) NULL); //Replace fork with bot.

        //Nothing past the execl should be run
        assert(false); //Changed to assert from exit(1) as it will fail loudly.
    } else if(pid < 0) {
        if(!quiet_output) std::cout << "Fork failed.\n";
        throw 1;
    }

    const int NUM_CON_MILLIS = 3000;
    int millisLeft = NUM_CON_MILLIS;
    std::chrono::high_resolution_clock::time_point tp = std::chrono::high_resolution_clock::now();
    while(millisLeft >= 0) {
        if(accept(sock, (struct sockaddr *)&clien_addr, &clien_addr_len) >= 0) break;
        millisLeft -= std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now() - tp).count();
    }
    if(millisLeft < 0) {
        if(!quiet_output) std::cout << "Player did not connect.\n";
        throw 2;
    }

    connections.push_back(sock);
    processes.push_back(pid);

#endif

    player_logs.push_back(std::string());
}

void Networking::handleInitNetworking(unsigned char playerTag, const hlt::Map & m, int * playermillis, std::string * playerName) {
    std::string response;
    try{
        std::string playerTagString = std::to_string(playerTag), mapSizeString = serializeMapSize(m), mapString = serializeMap(m), prodString = serializeProductions(m);
        sendString(playerTag, playerTagString);
        sendString(playerTag, mapSizeString);
        sendString(playerTag, prodString);
        sendString(playerTag, mapString);
        std::string outMessage = "Init Message sent to player " + std::to_string(int(playerTag)) + ".\n";
        if(!quiet_output) std::cout << outMessage;

        player_logs[playerTag - 1] += " --- Init --> Bot has " + std::to_string(*playermillis) + " milliseconds remaining ---\n";

        std::chrono::high_resolution_clock::time_point initialTime = std::chrono::high_resolution_clock::now();
        response = getString(playerTag, *playermillis);
        unsigned int millisTaken = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now() - initialTime).count();

        player_logs[playerTag - 1] += response + "\n --- Bot used " + std::to_string(millisTaken) + " milliseconds ---";

        *playerName = response.substr(0, 30);
        std::string inMessage = "Init Message received from player " + std::to_string(int(playerTag)) + ", " + *playerName + ".\n";
        if(!quiet_output) std::cout << inMessage;

        *playermillis -= millisTaken;
    }
    catch(std::string s) {
        if(s.empty()) player_logs[playerTag - 1] += "\nERRORED!\nNo response received.";
        else player_logs[playerTag - 1] += "\nERRORED!\nResponse received (if any):\n" + s;
        *playerName = "Bot #" + std::to_string(playerTag) + "; timed out during Init";
        *playermillis = -1;
    }
    catch(...) {
        if(response.empty()) player_logs[playerTag - 1] += "\nERRORED!\nNo response received.";
        else player_logs[playerTag - 1] += "\nERRORED!\nResponse received (if any):\n" + response;
        *playerName = "Bot #" + std::to_string(playerTag) + "; timed out during Init";
        *playermillis = -1;
    }
}

void Networking::handleFrameNetworking(unsigned char playerTag, const unsigned short & turnNumber, const hlt::Map & m, int * playermillis, std::map<hlt::Location, unsigned char> * moves) {
    std::string response;
    try{
        if(isProcessDead(playerTag)) return;

        //Send this bot the game map and the messages addressed to this bot
        std::string mapString = serializeMap(m);
        sendString(playerTag, mapString);

        moves->clear();

        player_logs[playerTag - 1] += "\n-----------------------------------------------------------------------------\n --- Frame #" + std::to_string(turnNumber) + " --> Bot has " + std::to_string(*playermillis) + " milliseconds remaining ---\n";

        std::chrono::high_resolution_clock::time_point initialTime = std::chrono::high_resolution_clock::now();
        response = getString(playerTag, *playermillis);
        unsigned int millisTaken = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now() - initialTime).count();

        player_logs[playerTag - 1] += response + "\n --- Bot used " + std::to_string(millisTaken) + " milliseconds ---";

        *moves = deserializeMoveSet(response, m);

        *playermillis -= millisTaken;
    }
    catch(std::string s) {
        if(s.empty()) player_logs[playerTag - 1] += "\nERRORED!\nNo response received.";
        else player_logs[playerTag - 1] += "\nERRORED!\nResponse received (if any):\n" + s;
        *moves = std::map<hlt::Location, unsigned char>();
        *playermillis = -1;
    }
    catch(...) {
        if(response.empty()) player_logs[playerTag - 1] += "\nERRORED!\nNo response received.";
        else player_logs[playerTag - 1] += "\nERRORED!\nResponse received (if any):\n" + response;
        *moves = std::map<hlt::Location, unsigned char>();
        *playermillis = -1;
    }

}

void Networking::killPlayer(unsigned char playerTag) {
    if(isProcessDead(playerTag)) return;
#ifdef _WIN32

    HANDLE process = processes[playerTag - 1];

    TerminateProcess(process, 0);

    processes[playerTag - 1] = NULL;
    connections[playerTag - 1] = NULL;

    if(!quiet_output) std::cout << "Player " << int(playerTag) << " is dead\n";
#else
    kill(-processes[playerTag - 1], SIGKILL);

    processes[playerTag - 1] = -1;
    connections[playerTag - 1] = -1;
#endif
}

bool Networking::isProcessDead(unsigned char playerTag) {
#ifdef _WIN32
    return processes[playerTag - 1] == NULL;
#else
    return processes[playerTag - 1] == -1;
#endif
}

int Networking::numberOfPlayers() {
#ifdef _WIN32
    return connections.size();
#else
    return connections.size();
#endif
}
