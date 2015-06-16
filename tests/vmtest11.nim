
import stdobj,cmodel
import options,kwdgrammar

defineMessage(cxComponent, "define:as:") 
do (msg,blck):
  let m = aggxCompiledMethod.instantiate
  let pBlock = blck.dataPtr(Block)
  assert(not pBlock.isNil)
  blck.printcomponents

  # define a CompiledMethod out of the bytecode for this blck
  let method_owner = pBlock.meth
  let len = pBlock.ipEnd - pBlock.ipStart + 1
  let ipSource =
    cast[ptr UncheckedArray[char]](
      method_owner.dataPtr(CompiledMethod).bytecode[0].addr
    )
  var iseq = newSeq[byte](len)
  copyMem iseq[0].addr, ipSource + pBlock.ipStart, len

  # copy out args and locals
  # TODO this sucks, but this only has to happen one other place thats 
  # at block instantiation (stdobj.createBlockContext)
  # instead when a method is created it should include contexts for sub-blocks
  var locals = [
    newSeq[string](pBlock.nargs),
    newSeq[string](pBlock.nlocals)
  ]
  let locals_ip = ipSource + pBlock.argNamesAt
  var tab_id, arg_idx = 0
  var cur = ""
  for i in 0 .. < pBlock.argNamesBytes:
    let c = locals_ip[i]
    if c == '\00':
      arg_idx += 1
      if arg_idx > locals[tab_id].high:
        tab_id += 1
        arg_idx = 0
    else:
      locals[tab_id][arg_idx].safeAdd(c)

  result = aggxCompiledMethod.instantiate
  result.dataVar(CompiledMethod) = 
    initCompiledMethod(
      iseq,
      args = locals[0],
      locals = locals[1])

  this.dataVar(Component).rawDefine msg.asString[], result



const testStr = """
Components Int define: 'test1' as: [
  1111
]
"""

const testStr2 = """
42 test1
"""

proc main =
  let r = execute(testStr)
  echo "-----------"
  r.printcomponents
  echo "--------------------"
  assert execute(testStr2).dataVar(int) == 1111
  #echo r.dataPtr(CompiledMethod).bytecode


  # let x = stdobj.parseExpression(testStr2)
  # echo x
  # echo "------------"


  #let ob1 = asObject(42).send("test1")
  # ob1.printcomponents
  # assert ob1.dataVar(int) == 1111
  #execute(testStr).send("value").dataVar(int) == 3
  # let ob1 = execute(testStr)
  # ob1.printcomponents
  # let ob2 = ob1.send("value")
  # ob2.printcomponents
  # assert ob2.dataVar(int) == 3

main()
