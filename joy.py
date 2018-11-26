# Licensed under Mit License -- Copyright (c) 2018 Strick Yak (a.k.a. Henry Strickland)

from go import regexp

OPEN = regexp.MustCompile('[[]')  # just '['
CLOSE = regexp.MustCompile('[]]') # just ']'
DEFINITION = regexp.MustCompile(`^\s*([-A-Za-z0-9_]+)\s*[=][=](.*)$`)

def IsANumber(x):
  try:
    float(str(x))
    return True
  except:
    pass
  return False

class Thing:
  def __repr__():
    return str(self)
    return '(%T:%s)' % (self, self)
  def Eval(j):
    j.Push(self)
  def MustSymbol():
    raise 'Must be a Symbol, but got: %T = %s' % (self, self)
  def MustNumber():
    raise 'Must be a Number, but got: %T = %s' % (self, self)
  def MustList():
    raise 'Must be a List, but got: %T = %s' % (self, self)

class Symbol(Thing):
  def __init__(s, fn):
    .s = str(s)
    .fn = fn
  def __str__():
    return .s
  def Eval(j):
    .fn(j)
  def MustSymbol():
    return self
  def Null_():
    raise '`null` not defined on a Symbol.'
  def Small_():
    raise '`small` not defined on a Symbol.'

class Number(Thing):
  def __init__(x):
    .x = float(x)
  def __str__():
    return str(.x)
  def MustNumber():
    return self
  def Null_():
    return .x == 0.0
  def Small_():
    return .x < 2.0
  
class List(Thing):
  def __init__(v):
    .v = list(v)
  def __str__():
    return '[ ' + ' '.join([str(e) for e in .v]) + ' ]'
  def Apply(j):
    print '['
    i = 0
    for e in .v:
      j.Show(.v[i:])
      try:
        e.Eval(j)
      except as ex:
        raise '%s\n    ... during Eval() of %s' % (ex, e)
      i += 1
    print ']'
    j.Show([])
  def MustList():
    return self
  def Null_():
    return len(.v) == 0.0
  def Small_():
    return len(.v) < 2.0

class Joy:
  def __init__():
    .stack = []
  def Show(prog):
    print '%s  ~~~~  %s' % (' '.join([str(e) for e in .stack]),
                            ' '.join([str(e) for e in prog]))
  def Peek():
    if not .stack: raise 'Cannot Peek() on empty stack'
    return .stack[-1]
  def Pop():
    if not .stack: raise 'Cannot Pop() on empty stack'
    return .stack.pop()
  def Push(x :: Thing):
    .stack.append(x)

  def Run(s :: str):
    m = DEFINITION.FindStringSubmatch(s)
    if m:
      _, k, s = m
      print '*** DEFINING: %s == %s' % (k, s)
      l = Lex(s)
      p = Parse(l)
      SYMBOLS[k] = Symbol(k, lambda j: List(p).Apply(j))
    else:
      print '*** RUNNING: %s' % s
      l = Lex(s)
      p = Parse(l)
      List(p).Apply(self)
      z = ' '.join([str(e) for e in .stack])
      print '*** RESULT: %s' % z
      return z

def Lex(s :: str):
  s = OPEN.ReplaceAllString(s, ' [ ')   # add spaces around '['.
  s = CLOSE.ReplaceAllString(s, ' ] ')  # add spaces around ']'.
  z = [w for w in s.split(' ') if w]
  return z

def Parse(tokens :: list):
  def recurse(v :: list):
    z = []
    while v:
      a = v.pop(0)
      switch:
        case a=='[':
          b, rest = recurse(v)
          z.append(List(b))
          v = rest
        case a==']':
          return z, v
        case IsANumber(a):
          z.append(Number(a))
        default:
          if a not in SYMBOLS: raise 'Symbol not defined: ' + a
          z.append(SYMBOLS[a])
    return z, v
  z, rest = recurse(tokens)
  if rest: raise 'Unterminated block'
  return z
          
def SymbolList():
  def _swap(j):
    a, b = j.Pop(), j.Pop()
    j.Push(a)
    j.Push(b)
  def _concat(j):
    a, b = j.Pop().MustList().v, j.Pop().MustList().v
    j.Push(List(b + a))
  def _map(j):
    fn = j.Pop().MustList()
    vec = j.Pop().MustList().v
    z = []
    for e in vec:
      j.Push(e)
      fn.Apply(j)
      z.append(j.Pop())
    j.Push( List(z) )
  def _ifte(j):
    else_, then_, if_, value_ = j.Pop().MustList(), j.Pop().MustList(), j.Pop().MustList(), j.Pop()
    j.Push(value_)
    if_.Apply(j)
    cond = j.Pop().MustNumber().x
    j.Push(value_)
    if cond:
      then_.Apply(j)
    else:
      else_.Apply(j)
  def _linrec(j):
    r2, r1, t_, b_ = j.Pop().MustList(), j.Pop().MustList(), j.Pop().MustList(), j.Pop().MustList()
    def recurse():
      n_ = j.Pop().MustNumber()
      j.Push(n_), b_.Apply(j)
      if j.Pop().MustNumber().x:
        j.Push(n_), t_.Apply(j)
      else:
        j.Push(n_), r1.Apply(j)
        recurse()
        r2.Apply(j)
    recurse() 
  def _dip(j):
    tos = j.Pop()
    j.Pop().MustList().Apply(j)
    j.Push(tos)
  return [
      Symbol('swap', _swap),
      Symbol('ifte', _ifte),
      Symbol('map', _map),
      Symbol('concat', _concat),
      Symbol('linrec', _linrec),
      Symbol('dip', _dip),
      Symbol('null', lambda j: j.Push(Number(j.Pop().Null_()))),
      Symbol('small', lambda j: j.Push(Number(j.Pop().Small_()))),
      Symbol('!=', lambda j: j.Push(Number(j.Pop().MustNumber().x != j.Pop().MustNumber().x))),
      Symbol('==', lambda j: j.Push(Number(j.Pop().MustNumber().x == j.Pop().MustNumber().x))),
      Symbol('<', lambda j: j.Push(Number(j.Pop().MustNumber().x > j.Pop().MustNumber().x))),
      Symbol('<=', lambda j: j.Push(Number(j.Pop().MustNumber().x >= j.Pop().MustNumber().x))),
      Symbol('>', lambda j: j.Push(Number(j.Pop().MustNumber().x < j.Pop().MustNumber().x))),
      Symbol('>=', lambda j: j.Push(Number(j.Pop().MustNumber().x <= j.Pop().MustNumber().x))),
      Symbol('+', lambda j: j.Push(Number(j.Pop().MustNumber().x + j.Pop().MustNumber().x))),
      Symbol('-', lambda j: j.Push(Number(- j.Pop().MustNumber().x + j.Pop().MustNumber().x))),
      Symbol('*', lambda j: j.Push(Number(j.Pop().MustNumber().x * j.Pop().MustNumber().x))),
      Symbol('id', lambda j: None),
      Symbol('i', lambda j: j.Pop().MustList().Apply(j)),
      Symbol('x', lambda j: j.Peek().MustList().Apply(j)),
      Symbol('pop', lambda j: j.Pop()),
      Symbol('dup', lambda j: j.Push(j.Peek())),
      Symbol('succ', lambda j: j.Push(Number(j.Pop().MustNumber().x + 1))),
      Symbol('pred', lambda j: j.Push(Number(j.Pop().MustNumber().x - 1))),
      ]

SYMBOLS = dict([(str(e), e) for e in SymbolList()])

# (2 + 3) squared.
assert '25' == Joy().Run('''  2 3 + dup *  ''')
assert '25' == Joy().Run('''  2 id 3 id + id dup id *  ''')
assert '25' == Joy().Run('''  [2 3 + dup *] i  ''')
assert '25' == Joy().Run('''  [2 3 + dup *] x swap pop  ''')
assert '2500' == Joy().Run('''  [2 3 + dup *] 100 dip *   ''')
# concat two lists.
assert '[ 1 2 3 4 5 6 ]' == Joy().Run('''  [1 2 3 4] [5 6] concat   ''')
# map a squaring block over [1 2 3 4 5].
assert '[ 1 4 9 16 25 ]' == Joy().Run('''  [1 2 3 4 5] [dup *] map   ''')
# absolute value
assert '9.5' == Joy().Run('''  -9.5 [0 <]  [0 swap -]  []  ifte   ''')
assert '9.5' == Joy().Run('''  +9.5 [0 <]  [0 swap -]  []  ifte   ''')
# define `square`
Joy().Run('''  square  ==  dup *                                  ''')
assert '[ 1 4 9 16 25 ]' == Joy().Run('''  [1 2 3 4 5] [square] map   ''')
# define `factorial`
Joy().Run('''  factorial  ==  [null] [succ] [dup pred] [*] linrec    ''')
assert '720' == Joy().Run(''' 6 factorial ''')
