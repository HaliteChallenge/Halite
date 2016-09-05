from hlt import *
from networking import *
import sys

myID, gameMap = getInit(int(sys.argv[1])) # Port is first argument.
sendInit("PythonBot")

while True:
	moves = []
	gameMap = getFrame()
	for y in range(gameMap.height):
		for x in range(gameMap.width):
			if gameMap.getSite(Location(x, y)).owner == myID:
				moves.append(Move(Location(x, y), int(random.random() * 5)))
	sendFrame(moves)
