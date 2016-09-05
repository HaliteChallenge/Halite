#include <iostream>
#include <cctype>
#include <chrono>
#include <list>
#include <string.h>

#include "core/Halite.hpp"

bool quiet_output = false; //Need to be passed to the game.
Halite * my_game; //Is a pointer to avoid problems with assignment, dynamic memory, and default constructors.

Networking promptNetworking(const int DEF_PORT_NO);
void promptDimensions(unsigned short & w, unsigned short & h);

int main(int argc, char ** argv) {
	srand(time(NULL)); //For all non-seeded randomness.

	bool watch_game = false, override_names = false; //Extra parameters.

	//Paramters to start up a game.
	bool passed_dimensions = false, passed_seed = false, passed_bot_names = false, ignore_timeout = false;
	unsigned short mapWidth, mapHeight;
	unsigned int seed = (std::chrono::duration_cast<std::chrono::microseconds>(std::chrono::system_clock::now().time_since_epoch()).count() % 4294967295); //Using microseconds to prevent same maps from coming up due to multiple worker servers.
	Networking networking;
	std::vector<std::string> * names = NULL;
	std::string * ppmFilename = NULL;
	unsigned int id = std::chrono::duration_cast<std::chrono::seconds>(std::chrono::high_resolution_clock().now().time_since_epoch()).count();
	int DEFAULT_PORT_NO = 2000;

	std::list<std::string> sArgs;
	for(int a = 1; a < argc; a++) sArgs.push_back(argv[a]);

	for(auto a = sArgs.begin(); a != sArgs.end();) {
		if(*a == "-d") {
			passed_dimensions = true;
			a = sArgs.erase(a);
			try {
				if(a == sArgs.end()) throw 0;
				mapWidth = std::stoll(*a);
				a = sArgs.erase(a);
				if(a == sArgs.end()) throw 0;
				mapHeight = std::stoll(*a);
				a = sArgs.erase(a);
			}
			catch(...) {
				std::cout << "The dimension parameters were either not present or invalid despite the flag having been given." << std::endl;
				return EXIT_FAILURE;
			}
		}
		else if(*a == "-w") {
			watch_game = true;
			a = sArgs.erase(a);
		}
		else if(*a == "-q") {
			quiet_output = true;
			a = sArgs.erase(a);
		}
		else if(*a == "-o") {
			override_names = true;
			a = sArgs.erase(a);
		}
		else if(*a == "-s") {
			passed_seed = true;
			a = sArgs.erase(a);
			try {
				if(a == sArgs.end()) throw 0;
				seed = std::stoll(*a);
				a = sArgs.erase(a);
			}
			catch(...) {
				std::cout << "The seed parameter was either not present or invalid despite the flag having been given." << std::endl;
				return EXIT_FAILURE;
			}
		}
		else if(*a == "--godmode") {
			a = sArgs.erase(a);
			try {
				ppmFilename = new std::string(*a);
				a = sArgs.erase(a);
			}
			catch(...) {
				std::cout << "The godmode parameter was either not present or invalid despite the flag having been given." << std::endl;
				return EXIT_FAILURE;
			}
		}
		else if(*a == "-t") {
			ignore_timeout = true;
			a = sArgs.erase(a);
		}
		else if(*a == "-p") {
			a = sArgs.erase(a);
			try {
				if(a == sArgs.end()) throw 0;
				DEFAULT_PORT_NO = std::stoll(*a);
				a = sArgs.erase(a);
			}
			catch(...) {
				std::cout << "The port parameter was either not present or invalid despite the flag having been given." << std::endl;
				return EXIT_FAILURE;
			}
		}
		else a++;
	}

	if(!passed_dimensions) {
		promptDimensions(mapWidth, mapHeight);
	}

	if(override_names) {
		if(sArgs.size() < 4 || sArgs.size() % 2 != 0) {
			std::cout << "Invalid player parameters from argv. Prompting instead (override disabled):" << std::endl;
			networking = promptNetworking(DEFAULT_PORT_NO);
		}
		else {
			try {
				int portno = DEFAULT_PORT_NO;
				names = new std::vector<std::string>();
				while(!sArgs.empty()) {
					try {
						networking.startAndConnectBot(sArgs.front(), portno);
					}
					catch(int e) {
						if(e == 2) { //Player did not connect.
							continue; //Skip this player.
						}
					}
					sArgs.pop_front();
					names->push_back(sArgs.front());
					sArgs.pop_front();
					portno++;
				}
			}
			catch(...) {
				std::cout << "Invalid player parameters from argv. Prompting instead (override disabled):" << std::endl;
				networking = promptNetworking(DEFAULT_PORT_NO);
				delete names;
				names = NULL;
			}
		}
	}
	else {
		if(sArgs.size() < 2) {
			std::cout << "Invalid player parameters from argv. Prompting instead:" << std::endl;
			networking = promptNetworking(DEFAULT_PORT_NO);
		}
		try {
			int portno = DEFAULT_PORT_NO;
			while(!sArgs.empty()) {
				networking.startAndConnectBot(sArgs.front(), portno);
				sArgs.pop_front();
				portno++;
			}
		}
		catch(...) {
			std::cout << "Invalid player parameters from argv. Prompting instead:" << std::endl;
			networking = promptNetworking(DEFAULT_PORT_NO);
		}
	}

	//Create game. Null parameters will be ignored.
	my_game = new Halite(mapWidth, mapHeight, seed, networking, ignore_timeout, ppmFilename);


	GameStatistics stats = my_game->runGame(names, seed, id);
	if(names != NULL) delete names;

	std::string victoryOut;
	if(quiet_output) {
		std::cout << stats;
	}
	else {
		for(unsigned int a = 0; a < stats.player_statistics.size(); a++) std::cout << "Player #" << stats.player_statistics[a].tag << ", " << my_game->getName(stats.player_statistics[a].tag) << ", came in rank #" << stats.player_statistics[a].rank << "!\n";
	}

	delete my_game;

	if(watch_game) {
#ifdef _WIN32
		std::string command = ".\\visualizer " + stats.output_filename;
#else
		std::string command = "./visualizer " + stats.output_filename;
#endif
		system(command.c_str());
	}

	return 0;
}

Networking promptNetworking(const int DEF_PORT_NO) {
	Networking n;
	std::string in;
	int portno = DEF_PORT_NO;
	bool done = false;
	for(int np = 0; !done; np++) {
		//If less than 2, bypass this step: Ask if the user like to add another AI
		if (np >= 2) {
			std::cout << "Would you like to add another player? Please enter Yes or No: ";
			while (true) {
				std::getline(std::cin, in);
				std::transform(in.begin(), in.end(), in.begin(), ::tolower);
				if (in == "n" || in == "no" || in == "nope" || in == "y" || in == "yes" || in == "yep") break;
				std::cout << "That isn't a valid input. Please enter Yes or No: ";
			}
			if (in == "n" || in == "no" || in == "nope") break;
		}

		while (true) {
			std::string startCommand;
			std::cout << "What is the start command for this bot: ";
			std::getline(std::cin, startCommand);

			try{
				n.startAndConnectBot(startCommand, portno);
				break;
			}
			catch(...) {
				std::cout << "There was a problem with that start command. Please enter another one.\n";
			}
		}

		std::cout << "Connected to player #" << int(np + 1) << std::endl;
		portno++;
	}
	return n;
}

void promptDimensions(unsigned short & w, unsigned short & h) {
	std::string in;
	std::cout << "Please enter the width of the map: ";
	std::getline(std::cin, in);
	while(true) {
		try{
			w = std::stoi(in);
			break;
		}
		catch(std::exception e) {
			std::cout << "That isn't a valid input. Please enter a positive integer width of the map: ";
			std::getline(std::cin, in);
		}
	}
	std::cout << "Please enter the height of the map: ";
	std::getline(std::cin, in);
	while(true) {
		try{
			h = std::stoi(in);
			break;
		}
		catch(std::exception e) {
			std::cout << "That isn't a valid input. Please enter a positive integer height of the map: ";
			std::getline(std::cin, in);
		}
	}
}
