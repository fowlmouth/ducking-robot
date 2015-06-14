import gctester


type Ob1 = object
  my_str: string

gcTest:
  var my_s = "Hello"
  my_s.shallow

  var sx = newSeq[string](1)
  # shallowCopy sx[0], my_s
  # sx[0].add 'y'
  # assert my_s == "Helloy", my_s & ":" & sx[0]

  when false:
    exprIncsRef(my_s):
      sx.add my_s
    assert getRefCount(my_s) == 1

    exprDecsRef(my_s):
      discard sx.pop

    assert getRefCount(my_s) == 0