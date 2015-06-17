
import stdobj,cmodel
import options,kwdgrammar

defineMessage(cxComponent, "define:as:") 
do (msg,blck):
  let pBlock = blck.dataPtr(Block)
  assert(not pBlock.isNil)

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
].
Components Int define: 'test2:' as: [:arg|
  self + arg + 1
]
"""
const testStr2 = """
42 test1
"""

const testStr3 = """
"""
const testStr4 = """
1 test2: 2
"""

proc main =
  block:
    let r = execute(testStr)
    echo "-----------"
    r.printcomponents
    echo "--------------------"
    assert execute("42 test1").dataVar(int) == 1111
    assert execute("1 test2: 2").dataVar(int) == 4

  block:
    assert nil != execute("""
Components True define: 'ifTrue:' as: [:block|
  block value
].
Components False define: 'ifTrue:' as: [:block|
  nil
].
Components Block define: 'ifTrue:' as: [:block|
  self value ifTrue: block
]
""")
    let o = execute("[true] ifTrue: [1]")
    o.printcomponents


main()
