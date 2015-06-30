const testStr = "Lobby Lobby"

import stdobj,cmodel,vm,options

proc main =
  #execute(testStr).send("value").dataVar(int) == 3
  assert(execute("thisContext Lobby Lobby").unsafeGet == obj_lobby.send("Lobby"))

main()
