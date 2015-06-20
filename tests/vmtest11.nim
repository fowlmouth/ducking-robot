
import stdobj,cmodel
import options,kwdgrammar


assert isSome execute("""

Components Int define: 'factorial' as: [
  [self < 2] ifTrue: [^ 1].
  ^ self * (self-1) factorial
].


""")

proc main =
  block:
    
    echo "----------------------"

    # var o = execute("Lobby retry")
    # assert o == nil
    let print_backtrace = proc(exe:Object) =
      let e = exe.dataPtr(Exec)
      var ctx = e.activeContext
      var i = 0
      while not ctx.isNil:
        #echo ctx.safetype.printComponentNames 
        echo i,": ", ctx.findComponent("Locals").slotNames
        echo "  ", ctx.dataPtr(Context).instrs.dataPtr(CompiledMethod).name
        ctx = ctx.dataPtr(Context).caller

    discard "[x| x: 0. [x < 2] whileTrue: [x: x + 1]. ^x. 2] value"
    let ss = "[x| x: 0. [x < 2] whileTrue: [x: x + 1]. x printValue] value"

    # let o = execute(
    #   ss).unsafeget
    #   #"").unsafeget#, do_between = print_backtrace).unsafeget
    # #"[x| x:1. x<2 ifTrue: [^true]. ^false] value")
    # o.printcomponents
    # echo o.send("print").asString[]

    let ss2 = "5 factorial"
    let o = execute(ss2).unsafeget
    o.printcomponents
    echo o.send("print").asString[]

    # # should loop forever..
    # o = execute("retry")
    # o.printcomponents
    # echo "RESULT: ", o.asString[]

when isMainModule:
  main()
