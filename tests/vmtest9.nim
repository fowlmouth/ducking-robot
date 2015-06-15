const testStr = "Lobby Lobby"

import stdobj,cmodel

proc main =
  #execute(testStr).send("value").dataVar(int) == 3
  assert(execute("thisContext Lobby Lobby") == obj_lobby.send("Lobby"))

main()
