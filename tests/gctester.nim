discard """
a generic gc test?

what operations are required
  - an expression that saves a ref must increment its reference
  - an expression that replaces a ref must decrement the ref it held previously
  - a gc-safe type decrements held references upon destruction (manual or automatic)
"""

template gcTest* (body: untyped): stmt {. dirty, immediate } =
  template exprIncsRef (rf, xpr: untyped): stmt =
    {.line: instantiationInfo() }:
      block:
        let rc = getRefCount(rf)
        block: xpr
        GC_fullCollect()

        do_assert(
          not rf.isNil and getRefCount(rf) == rc+1, 
          "expression did not incref: "& astToStr(xpr) )

  template exprDecsRef (rf, xpr: untyped): stmt =
    {.line: instantiationInfo() }:
      block:
        do_assert(not rf.isNil, "reference is not valid! "& $instantiationInfo())
      
        let old_rf = rf
        let old_rc = getRefCount(rf)
        block: xpr
        GC_fullCollect()

        do_assert(
          getRefCount(old_rf) == old_rc-1, 
          "expression did not dec ref "& astToStr(xpr)
        )

  template exprReplacesRef (newRf, oldRf, xpr: untyped): stmt =
    {.line: instantiationInfo() }:
      exprIncsRef(newRf):
        exprDecsRef(oldRf):
          xpr
  
  template exprDoesntRef (oldRf, xpr: untyped): stmt =
    {.line: instantiationInfo() }:
      let old_rc = getRefCount(oldRf)
      xpr
      GC_fullCollect()
      do_assert getRefCount(oldRf) == old_rc, "expression did not keep ref "& astToStr(xpr)

  try:
    body

  except:
    echo("Unhandled exception: " & getCurrentExceptionMsg())
    echo getCurrentException().getStackTrace()

