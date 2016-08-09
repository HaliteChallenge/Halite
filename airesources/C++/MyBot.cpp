#include <stdlib.h>
#include <time.h>
#include <cstdlib>
#include <ctime>
#include <time.h>
#include <set>
#include <fstream>

#include "hlt.hpp"
#include "networking.hpp"

int main(int argc, char ** argv) {
	srand(time(NULL));

	std::ofstream dbg("dbg.log");

	std::cout.sync_with_stdio(0);

	unsigned char myID;
	hlt::GameMap presentMap;
	getInit(atoi(argv[1]), myID, presentMap); //argv[1] is the port number to connect to.
	sendInit("C++Bot");

	std::set<hlt::Move> moves;
	while(true) {
		moves.clear();

		getFrame(presentMap);

		for(unsigned short a = 0; a < presentMap.height; a++) {
			for(unsigned short b = 0; b < presentMap.width; b++) {
				if (presentMap.getSite({ b, a }).owner == myID) {
					moves.insert({ { b, a }, (unsigned char)(rand() % 5) });
				}
			}
		}

		sendFrame(moves);
	}

	return 0;
}
