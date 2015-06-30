const testStr = "[x| x: 4. [x - 1] ] value"

import stdobj,cmodel,vm,options

proc main =
  #execute(testStr).send("value").dataVar(int) == 3
  let ob1 = execute(testStr).unsafeGet
  ob1.printcomponents
  let ob2 = ob1.send("value")
  ob2.printcomponents
  assert ob2.dataVar(int) == 3

main()
