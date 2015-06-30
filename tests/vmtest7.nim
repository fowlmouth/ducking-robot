
import stdobj,vm,cmodel,options


proc main =
  let ob = execute("[x| x: 4. return: thisContext to: caller. x ] value").unsafeGet
  echo "--------"
  ob.printcomponents

  let ctxPtr = ob.dataPtr(Context)
  assert(not ctxPtr.isNil)


  echo "--------"

  ## can we make the vm continue from this context returned?
  let vm = executorForContext(ob)
  let exe = vm .dataPtr(Exec)
  while exe.isActive:
    #exe.tick
    tick(vm)
  let ob2 = exe.result
  echo "-------------------------------------------"
  ob2.printcomponents
  echo ob2.send("print").asString[]

  assert ob2.dataVar(int) == 4


main()

