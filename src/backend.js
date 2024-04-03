// output/Control.Semigroupoid/index.js
var semigroupoidFn = {
  compose: function(f) {
    return function(g) {
      return function(x) {
        return f(g(x));
      };
    };
  }
};

// output/Control.Category/index.js
var identity = function(dict) {
  return dict.identity;
};
var categoryFn = {
  identity: function(x) {
    return x;
  },
  Semigroupoid0: function() {
    return semigroupoidFn;
  }
};

// output/Data.Function/index.js
var flip = function(f) {
  return function(b) {
    return function(a) {
      return f(a)(b);
    };
  };
};

// output/Data.Functor/foreign.js
var arrayMap = function(f) {
  return function(arr) {
    var l = arr.length;
    var result = new Array(l);
    for (var i = 0; i < l; i++) {
      result[i] = f(arr[i]);
    }
    return result;
  };
};

// output/Data.Unit/foreign.js
var unit = void 0;

// output/Type.Proxy/index.js
var $$Proxy = /* @__PURE__ */ function() {
  function $$Proxy2() {
  }
  ;
  $$Proxy2.value = new $$Proxy2();
  return $$Proxy2;
}();

// output/Data.Functor/index.js
var map = function(dict) {
  return dict.map;
};
var functorArray = {
  map: arrayMap
};

// output/Control.Apply/index.js
var apply = function(dict) {
  return dict.apply;
};
var lift2 = function(dictApply) {
  var apply1 = apply(dictApply);
  var map10 = map(dictApply.Functor0());
  return function(f) {
    return function(a) {
      return function(b) {
        return apply1(map10(f)(a))(b);
      };
    };
  };
};

// output/Control.Applicative/index.js
var pure = function(dict) {
  return dict.pure;
};

// output/Control.Bind/index.js
var bind = function(dict) {
  return dict.bind;
};

// output/Data.Semigroup/foreign.js
var concatString = function(s1) {
  return function(s2) {
    return s1 + s2;
  };
};
var concatArray = function(xs) {
  return function(ys) {
    if (xs.length === 0)
      return ys;
    if (ys.length === 0)
      return xs;
    return xs.concat(ys);
  };
};

// output/Data.Symbol/index.js
var reflectSymbol = function(dict) {
  return dict.reflectSymbol;
};

// output/Data.Semigroup/index.js
var semigroupString = {
  append: concatString
};
var semigroupArray = {
  append: concatArray
};
var append = function(dict) {
  return dict.append;
};

// output/Data.Bounded/foreign.js
var topChar = String.fromCharCode(65535);
var bottomChar = String.fromCharCode(0);
var topNumber = Number.POSITIVE_INFINITY;
var bottomNumber = Number.NEGATIVE_INFINITY;

// output/Data.Ord/foreign.js
var unsafeCompareImpl = function(lt) {
  return function(eq3) {
    return function(gt) {
      return function(x) {
        return function(y) {
          return x < y ? lt : x === y ? eq3 : gt;
        };
      };
    };
  };
};
var ordIntImpl = unsafeCompareImpl;
var ordStringImpl = unsafeCompareImpl;

// output/Data.Eq/foreign.js
var refEq = function(r1) {
  return function(r2) {
    return r1 === r2;
  };
};
var eqIntImpl = refEq;
var eqStringImpl = refEq;

// output/Data.Eq/index.js
var eqString = {
  eq: eqStringImpl
};
var eqInt = {
  eq: eqIntImpl
};
var eq = function(dict) {
  return dict.eq;
};

// output/Data.Ordering/index.js
var LT = /* @__PURE__ */ function() {
  function LT2() {
  }
  ;
  LT2.value = new LT2();
  return LT2;
}();
var GT = /* @__PURE__ */ function() {
  function GT2() {
  }
  ;
  GT2.value = new GT2();
  return GT2;
}();
var EQ = /* @__PURE__ */ function() {
  function EQ2() {
  }
  ;
  EQ2.value = new EQ2();
  return EQ2;
}();

// output/Data.Ring/foreign.js
var intSub = function(x) {
  return function(y) {
    return x - y | 0;
  };
};

// output/Data.Semiring/foreign.js
var intAdd = function(x) {
  return function(y) {
    return x + y | 0;
  };
};
var intMul = function(x) {
  return function(y) {
    return x * y | 0;
  };
};

// output/Data.Semiring/index.js
var semiringInt = {
  add: intAdd,
  zero: 0,
  mul: intMul,
  one: 1
};

// output/Data.Ring/index.js
var ringInt = {
  sub: intSub,
  Semiring0: function() {
    return semiringInt;
  }
};

// output/Data.Ord/index.js
var ordString = /* @__PURE__ */ function() {
  return {
    compare: ordStringImpl(LT.value)(EQ.value)(GT.value),
    Eq0: function() {
      return eqString;
    }
  };
}();
var ordInt = /* @__PURE__ */ function() {
  return {
    compare: ordIntImpl(LT.value)(EQ.value)(GT.value),
    Eq0: function() {
      return eqInt;
    }
  };
}();
var compare = function(dict) {
  return dict.compare;
};

// output/Data.Show/foreign.js
var showIntImpl = function(n) {
  return n.toString();
};
var showStringImpl = function(s) {
  var l = s.length;
  return '"' + s.replace(
    /[\0-\x1F\x7F"\\]/g,
    // eslint-disable-line no-control-regex
    function(c, i) {
      switch (c) {
        case '"':
        case "\\":
          return "\\" + c;
        case "\x07":
          return "\\a";
        case "\b":
          return "\\b";
        case "\f":
          return "\\f";
        case "\n":
          return "\\n";
        case "\r":
          return "\\r";
        case "	":
          return "\\t";
        case "\v":
          return "\\v";
      }
      var k = i + 1;
      var empty3 = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
      return "\\" + c.charCodeAt(0).toString(10) + empty3;
    }
  ) + '"';
};

// output/Data.Show/index.js
var showString = {
  show: showStringImpl
};
var showInt = {
  show: showIntImpl
};
var showBoolean = {
  show: function(v) {
    if (v) {
      return "true";
    }
    ;
    if (!v) {
      return "false";
    }
    ;
    throw new Error("Failed pattern match at Data.Show (line 29, column 1 - line 31, column 23): " + [v.constructor.name]);
  }
};
var show = function(dict) {
  return dict.show;
};

// output/Data.Generic.Rep/index.js
var Inl = /* @__PURE__ */ function() {
  function Inl2(value0) {
    this.value0 = value0;
  }
  ;
  Inl2.create = function(value0) {
    return new Inl2(value0);
  };
  return Inl2;
}();
var Inr = /* @__PURE__ */ function() {
  function Inr2(value0) {
    this.value0 = value0;
  }
  ;
  Inr2.create = function(value0) {
    return new Inr2(value0);
  };
  return Inr2;
}();
var Product = /* @__PURE__ */ function() {
  function Product2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Product2.create = function(value0) {
    return function(value1) {
      return new Product2(value0, value1);
    };
  };
  return Product2;
}();
var NoArguments = /* @__PURE__ */ function() {
  function NoArguments2() {
  }
  ;
  NoArguments2.value = new NoArguments2();
  return NoArguments2;
}();
var from = function(dict) {
  return dict.from;
};

// output/Data.Maybe/index.js
var Nothing = /* @__PURE__ */ function() {
  function Nothing2() {
  }
  ;
  Nothing2.value = new Nothing2();
  return Nothing2;
}();
var Just = /* @__PURE__ */ function() {
  function Just2(value0) {
    this.value0 = value0;
  }
  ;
  Just2.create = function(value0) {
    return new Just2(value0);
  };
  return Just2;
}();
var functorMaybe = {
  map: function(v) {
    return function(v1) {
      if (v1 instanceof Just) {
        return new Just(v(v1.value0));
      }
      ;
      return Nothing.value;
    };
  }
};
var map2 = /* @__PURE__ */ map(functorMaybe);
var eqMaybe = function(dictEq) {
  var eq3 = eq(dictEq);
  return {
    eq: function(x) {
      return function(y) {
        if (x instanceof Nothing && y instanceof Nothing) {
          return true;
        }
        ;
        if (x instanceof Just && y instanceof Just) {
          return eq3(x.value0)(y.value0);
        }
        ;
        return false;
      };
    }
  };
};
var ordMaybe = function(dictOrd) {
  var compare2 = compare(dictOrd);
  var eqMaybe1 = eqMaybe(dictOrd.Eq0());
  return {
    compare: function(x) {
      return function(y) {
        if (x instanceof Nothing && y instanceof Nothing) {
          return EQ.value;
        }
        ;
        if (x instanceof Nothing) {
          return LT.value;
        }
        ;
        if (y instanceof Nothing) {
          return GT.value;
        }
        ;
        if (x instanceof Just && y instanceof Just) {
          return compare2(x.value0)(y.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 0, column 0 - line 0, column 0): " + [x.constructor.name, y.constructor.name]);
      };
    },
    Eq0: function() {
      return eqMaybe1;
    }
  };
};
var applyMaybe = {
  apply: function(v) {
    return function(v1) {
      if (v instanceof Just) {
        return map2(v.value0)(v1);
      }
      ;
      if (v instanceof Nothing) {
        return Nothing.value;
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): " + [v.constructor.name, v1.constructor.name]);
    };
  },
  Functor0: function() {
    return functorMaybe;
  }
};
var applicativeMaybe = /* @__PURE__ */ function() {
  return {
    pure: Just.create,
    Apply0: function() {
      return applyMaybe;
    }
  };
}();

// output/Data.Either/index.js
var Left = /* @__PURE__ */ function() {
  function Left2(value0) {
    this.value0 = value0;
  }
  ;
  Left2.create = function(value0) {
    return new Left2(value0);
  };
  return Left2;
}();
var Right = /* @__PURE__ */ function() {
  function Right2(value0) {
    this.value0 = value0;
  }
  ;
  Right2.create = function(value0) {
    return new Right2(value0);
  };
  return Right2;
}();
var functorEither = {
  map: function(f) {
    return function(m) {
      if (m instanceof Left) {
        return new Left(m.value0);
      }
      ;
      if (m instanceof Right) {
        return new Right(f(m.value0));
      }
      ;
      throw new Error("Failed pattern match at Data.Either (line 0, column 0 - line 0, column 0): " + [m.constructor.name]);
    };
  }
};
var map3 = /* @__PURE__ */ map(functorEither);
var either = function(v) {
  return function(v1) {
    return function(v2) {
      if (v2 instanceof Left) {
        return v(v2.value0);
      }
      ;
      if (v2 instanceof Right) {
        return v1(v2.value0);
      }
      ;
      throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
    };
  };
};
var applyEither = {
  apply: function(v) {
    return function(v1) {
      if (v instanceof Left) {
        return new Left(v.value0);
      }
      ;
      if (v instanceof Right) {
        return map3(v.value0)(v1);
      }
      ;
      throw new Error("Failed pattern match at Data.Either (line 70, column 1 - line 72, column 30): " + [v.constructor.name, v1.constructor.name]);
    };
  },
  Functor0: function() {
    return functorEither;
  }
};
var bindEither = {
  bind: /* @__PURE__ */ either(function(e) {
    return function(v) {
      return new Left(e);
    };
  })(function(a) {
    return function(f) {
      return f(a);
    };
  }),
  Apply0: function() {
    return applyEither;
  }
};
var applicativeEither = /* @__PURE__ */ function() {
  return {
    pure: Right.create,
    Apply0: function() {
      return applyEither;
    }
  };
}();
var monadEither = {
  Applicative0: function() {
    return applicativeEither;
  },
  Bind1: function() {
    return bindEither;
  }
};

// output/Data.EuclideanRing/foreign.js
var intDegree = function(x) {
  return Math.min(Math.abs(x), 2147483647);
};
var intDiv = function(x) {
  return function(y) {
    if (y === 0)
      return 0;
    return y > 0 ? Math.floor(x / y) : -Math.floor(x / -y);
  };
};
var intMod = function(x) {
  return function(y) {
    if (y === 0)
      return 0;
    var yy = Math.abs(y);
    return (x % yy + yy) % yy;
  };
};

// output/Data.CommutativeRing/index.js
var commutativeRingInt = {
  Ring0: function() {
    return ringInt;
  }
};

// output/Data.EuclideanRing/index.js
var mod = function(dict) {
  return dict.mod;
};
var euclideanRingInt = {
  degree: intDegree,
  div: intDiv,
  mod: intMod,
  CommutativeRing0: function() {
    return commutativeRingInt;
  }
};
var div = function(dict) {
  return dict.div;
};

// output/Data.Foldable/foreign.js
var foldrArray = function(f) {
  return function(init) {
    return function(xs) {
      var acc = init;
      var len = xs.length;
      for (var i = len - 1; i >= 0; i--) {
        acc = f(xs[i])(acc);
      }
      return acc;
    };
  };
};
var foldlArray = function(f) {
  return function(init) {
    return function(xs) {
      var acc = init;
      var len = xs.length;
      for (var i = 0; i < len; i++) {
        acc = f(acc)(xs[i]);
      }
      return acc;
    };
  };
};

// output/Data.HeytingAlgebra/foreign.js
var boolConj = function(b1) {
  return function(b2) {
    return b1 && b2;
  };
};
var boolDisj = function(b1) {
  return function(b2) {
    return b1 || b2;
  };
};
var boolNot = function(b) {
  return !b;
};

// output/Data.HeytingAlgebra/index.js
var tt = function(dict) {
  return dict.tt;
};
var not = function(dict) {
  return dict.not;
};
var disj = function(dict) {
  return dict.disj;
};
var heytingAlgebraBoolean = {
  ff: false,
  tt: true,
  implies: function(a) {
    return function(b) {
      return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a))(b);
    };
  },
  conj: boolConj,
  disj: boolDisj,
  not: boolNot
};
var conj = function(dict) {
  return dict.conj;
};

// output/Data.Monoid/index.js
var monoidString = {
  mempty: "",
  Semigroup0: function() {
    return semigroupString;
  }
};
var mempty = function(dict) {
  return dict.mempty;
};

// output/Data.Tuple/index.js
var Tuple = /* @__PURE__ */ function() {
  function Tuple2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Tuple2.create = function(value0) {
    return function(value1) {
      return new Tuple2(value0, value1);
    };
  };
  return Tuple2;
}();
var snd = function(v) {
  return v.value1;
};

// output/Data.Monoid.Conj/index.js
var Conj = function(x) {
  return x;
};
var semigroupConj = function(dictHeytingAlgebra) {
  var conj2 = conj(dictHeytingAlgebra);
  return {
    append: function(v) {
      return function(v1) {
        return conj2(v)(v1);
      };
    }
  };
};
var monoidConj = function(dictHeytingAlgebra) {
  var semigroupConj1 = semigroupConj(dictHeytingAlgebra);
  return {
    mempty: tt(dictHeytingAlgebra),
    Semigroup0: function() {
      return semigroupConj1;
    }
  };
};

// output/Unsafe.Coerce/foreign.js
var unsafeCoerce2 = function(x) {
  return x;
};

// output/Safe.Coerce/index.js
var coerce = function() {
  return unsafeCoerce2;
};

// output/Data.Newtype/index.js
var coerce2 = /* @__PURE__ */ coerce();
var unwrap = function() {
  return coerce2;
};
var alaF = function() {
  return function() {
    return function() {
      return function() {
        return function(v) {
          return coerce2;
        };
      };
    };
  };
};

// output/Data.Foldable/index.js
var alaF2 = /* @__PURE__ */ alaF()()()();
var foldr = function(dict) {
  return dict.foldr;
};
var foldl = function(dict) {
  return dict.foldl;
};
var intercalate = function(dictFoldable) {
  var foldl22 = foldl(dictFoldable);
  return function(dictMonoid) {
    var append3 = append(dictMonoid.Semigroup0());
    var mempty2 = mempty(dictMonoid);
    return function(sep) {
      return function(xs) {
        var go = function(v) {
          return function(v1) {
            if (v.init) {
              return {
                init: false,
                acc: v1
              };
            }
            ;
            return {
              init: false,
              acc: append3(v.acc)(append3(sep)(v1))
            };
          };
        };
        return foldl22(go)({
          init: true,
          acc: mempty2
        })(xs).acc;
      };
    };
  };
};
var foldMapDefaultR = function(dictFoldable) {
  var foldr22 = foldr(dictFoldable);
  return function(dictMonoid) {
    var append3 = append(dictMonoid.Semigroup0());
    var mempty2 = mempty(dictMonoid);
    return function(f) {
      return foldr22(function(x) {
        return function(acc) {
          return append3(f(x))(acc);
        };
      })(mempty2);
    };
  };
};
var foldableArray = {
  foldr: foldrArray,
  foldl: foldlArray,
  foldMap: function(dictMonoid) {
    return foldMapDefaultR(foldableArray)(dictMonoid);
  }
};
var foldMap = function(dict) {
  return dict.foldMap;
};
var all = function(dictFoldable) {
  var foldMap2 = foldMap(dictFoldable);
  return function(dictHeytingAlgebra) {
    return alaF2(Conj)(foldMap2(monoidConj(dictHeytingAlgebra)));
  };
};

// output/Data.Int/foreign.js
var fromStringAsImpl = function(just) {
  return function(nothing) {
    return function(radix) {
      var digits;
      if (radix < 11) {
        digits = "[0-" + (radix - 1).toString() + "]";
      } else if (radix === 11) {
        digits = "[0-9a]";
      } else {
        digits = "[0-9a-" + String.fromCharCode(86 + radix) + "]";
      }
      var pattern = new RegExp("^[\\+\\-]?" + digits + "+$", "i");
      return function(s) {
        if (pattern.test(s)) {
          var i = parseInt(s, radix);
          return (i | 0) === i ? just(i) : nothing;
        } else {
          return nothing;
        }
      };
    };
  };
};
var pow = function(x) {
  return function(y) {
    return Math.pow(x, y) | 0;
  };
};

// output/Data.Int/index.js
var fromStringAs = /* @__PURE__ */ function() {
  return fromStringAsImpl(Just.create)(Nothing.value);
}();
var fromString = /* @__PURE__ */ fromStringAs(10);

// output/Data.Lazy/foreign.js
var defer2 = function(thunk) {
  var v = null;
  return function() {
    if (thunk === void 0)
      return v;
    v = thunk();
    thunk = void 0;
    return v;
  };
};
var force = function(l) {
  return l();
};

// output/Data.Traversable/foreign.js
var traverseArrayImpl = function() {
  function array1(a) {
    return [a];
  }
  function array2(a) {
    return function(b) {
      return [a, b];
    };
  }
  function array3(a) {
    return function(b) {
      return function(c) {
        return [a, b, c];
      };
    };
  }
  function concat2(xs) {
    return function(ys) {
      return xs.concat(ys);
    };
  }
  return function(apply2) {
    return function(map10) {
      return function(pure5) {
        return function(f) {
          return function(array) {
            function go(bot, top2) {
              switch (top2 - bot) {
                case 0:
                  return pure5([]);
                case 1:
                  return map10(array1)(f(array[bot]));
                case 2:
                  return apply2(map10(array2)(f(array[bot])))(f(array[bot + 1]));
                case 3:
                  return apply2(apply2(map10(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                default:
                  var pivot = bot + Math.floor((top2 - bot) / 4) * 2;
                  return apply2(map10(concat2)(go(bot, pivot)))(go(pivot, top2));
              }
            }
            return go(0, array.length);
          };
        };
      };
    };
  };
}();

// output/Data.Identity/index.js
var Identity = function(x) {
  return x;
};
var functorIdentity = {
  map: function(f) {
    return function(m) {
      return f(m);
    };
  }
};
var applyIdentity = {
  apply: function(v) {
    return function(v1) {
      return v(v1);
    };
  },
  Functor0: function() {
    return functorIdentity;
  }
};
var bindIdentity = {
  bind: function(v) {
    return function(f) {
      return f(v);
    };
  },
  Apply0: function() {
    return applyIdentity;
  }
};
var applicativeIdentity = {
  pure: Identity,
  Apply0: function() {
    return applyIdentity;
  }
};
var monadIdentity = {
  Applicative0: function() {
    return applicativeIdentity;
  },
  Bind1: function() {
    return bindIdentity;
  }
};

// output/Data.Traversable/index.js
var identity2 = /* @__PURE__ */ identity(categoryFn);
var traverse = function(dict) {
  return dict.traverse;
};
var sequenceDefault = function(dictTraversable) {
  var traverse2 = traverse(dictTraversable);
  return function(dictApplicative) {
    return traverse2(dictApplicative)(identity2);
  };
};
var traversableArray = {
  traverse: function(dictApplicative) {
    var Apply0 = dictApplicative.Apply0();
    return traverseArrayImpl(apply(Apply0))(map(Apply0.Functor0()))(pure(dictApplicative));
  },
  sequence: function(dictApplicative) {
    return sequenceDefault(traversableArray)(dictApplicative);
  },
  Functor0: function() {
    return functorArray;
  },
  Foldable1: function() {
    return foldableArray;
  }
};
var sequence = function(dict) {
  return dict.sequence;
};

// output/Data.Lazy/index.js
var functorLazy = {
  map: function(f) {
    return function(l) {
      return defer2(function(v) {
        return f(force(l));
      });
    };
  }
};
var applyLazy = {
  apply: function(f) {
    return function(x) {
      return defer2(function(v) {
        return force(f)(force(x));
      });
    };
  },
  Functor0: function() {
    return functorLazy;
  }
};
var applicativeLazy = {
  pure: function(a) {
    return defer2(function(v) {
      return a;
    });
  },
  Apply0: function() {
    return applyLazy;
  }
};

// output/Control.Monad/index.js
var ap = function(dictMonad) {
  var bind4 = bind(dictMonad.Bind1());
  var pure5 = pure(dictMonad.Applicative0());
  return function(f) {
    return function(a) {
      return bind4(f)(function(f$prime) {
        return bind4(a)(function(a$prime) {
          return pure5(f$prime(a$prime));
        });
      });
    };
  };
};

// output/Effect.Ref/foreign.js
var _new = function(val) {
  return function() {
    return { value: val };
  };
};
var read = function(ref) {
  return function() {
    return ref.value;
  };
};
var write = function(val) {
  return function(ref) {
    return function() {
      ref.value = val;
    };
  };
};

// output/Effect.Ref/index.js
var $$new = _new;

// output/Data.Unfoldable/index.js
var unfoldr = function(dict) {
  return dict.unfoldr;
};

// output/Data.List.Types/index.js
var identity3 = /* @__PURE__ */ identity(categoryFn);
var Nil = /* @__PURE__ */ function() {
  function Nil2() {
  }
  ;
  Nil2.value = new Nil2();
  return Nil2;
}();
var Cons = /* @__PURE__ */ function() {
  function Cons2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Cons2.create = function(value0) {
    return function(value1) {
      return new Cons2(value0, value1);
    };
  };
  return Cons2;
}();
var listMap = function(f) {
  var chunkedRevMap = function($copy_v) {
    return function($copy_v1) {
      var $tco_var_v = $copy_v;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v, v1) {
        if (v1 instanceof Cons && (v1.value1 instanceof Cons && v1.value1.value1 instanceof Cons)) {
          $tco_var_v = new Cons(v1, v);
          $copy_v1 = v1.value1.value1.value1;
          return;
        }
        ;
        var unrolledMap = function(v2) {
          if (v2 instanceof Cons && (v2.value1 instanceof Cons && v2.value1.value1 instanceof Nil)) {
            return new Cons(f(v2.value0), new Cons(f(v2.value1.value0), Nil.value));
          }
          ;
          if (v2 instanceof Cons && v2.value1 instanceof Nil) {
            return new Cons(f(v2.value0), Nil.value);
          }
          ;
          return Nil.value;
        };
        var reverseUnrolledMap = function($copy_v2) {
          return function($copy_v3) {
            var $tco_var_v2 = $copy_v2;
            var $tco_done1 = false;
            var $tco_result2;
            function $tco_loop2(v2, v3) {
              if (v2 instanceof Cons && (v2.value0 instanceof Cons && (v2.value0.value1 instanceof Cons && v2.value0.value1.value1 instanceof Cons))) {
                $tco_var_v2 = v2.value1;
                $copy_v3 = new Cons(f(v2.value0.value0), new Cons(f(v2.value0.value1.value0), new Cons(f(v2.value0.value1.value1.value0), v3)));
                return;
              }
              ;
              $tco_done1 = true;
              return v3;
            }
            ;
            while (!$tco_done1) {
              $tco_result2 = $tco_loop2($tco_var_v2, $copy_v3);
            }
            ;
            return $tco_result2;
          };
        };
        $tco_done = true;
        return reverseUnrolledMap(v)(unrolledMap(v1));
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_v, $copy_v1);
      }
      ;
      return $tco_result;
    };
  };
  return chunkedRevMap(Nil.value);
};
var functorList = {
  map: listMap
};
var foldableList = {
  foldr: function(f) {
    return function(b) {
      var rev = function() {
        var go = function($copy_v) {
          return function($copy_v1) {
            var $tco_var_v = $copy_v;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(v, v1) {
              if (v1 instanceof Nil) {
                $tco_done = true;
                return v;
              }
              ;
              if (v1 instanceof Cons) {
                $tco_var_v = new Cons(v1.value0, v);
                $copy_v1 = v1.value1;
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): " + [v.constructor.name, v1.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_v, $copy_v1);
            }
            ;
            return $tco_result;
          };
        };
        return go(Nil.value);
      }();
      var $284 = foldl(foldableList)(flip(f))(b);
      return function($285) {
        return $284(rev($285));
      };
    };
  },
  foldl: function(f) {
    var go = function($copy_b) {
      return function($copy_v) {
        var $tco_var_b = $copy_b;
        var $tco_done1 = false;
        var $tco_result;
        function $tco_loop(b, v) {
          if (v instanceof Nil) {
            $tco_done1 = true;
            return b;
          }
          ;
          if (v instanceof Cons) {
            $tco_var_b = f(b)(v.value0);
            $copy_v = v.value1;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done1) {
          $tco_result = $tco_loop($tco_var_b, $copy_v);
        }
        ;
        return $tco_result;
      };
    };
    return go;
  },
  foldMap: function(dictMonoid) {
    var append22 = append(dictMonoid.Semigroup0());
    var mempty2 = mempty(dictMonoid);
    return function(f) {
      return foldl(foldableList)(function(acc) {
        var $286 = append22(acc);
        return function($287) {
          return $286(f($287));
        };
      })(mempty2);
    };
  }
};
var foldl2 = /* @__PURE__ */ foldl(foldableList);
var foldr2 = /* @__PURE__ */ foldr(foldableList);
var semigroupList = {
  append: function(xs) {
    return function(ys) {
      return foldr2(Cons.create)(ys)(xs);
    };
  }
};
var traversableList = {
  traverse: function(dictApplicative) {
    var Apply0 = dictApplicative.Apply0();
    var map12 = map(Apply0.Functor0());
    var lift22 = lift2(Apply0);
    var pure13 = pure(dictApplicative);
    return function(f) {
      var $301 = map12(foldl2(flip(Cons.create))(Nil.value));
      var $302 = foldl2(function(acc) {
        var $304 = lift22(flip(Cons.create))(acc);
        return function($305) {
          return $304(f($305));
        };
      })(pure13(Nil.value));
      return function($303) {
        return $301($302($303));
      };
    };
  },
  sequence: function(dictApplicative) {
    return traverse(traversableList)(dictApplicative)(identity3);
  },
  Functor0: function() {
    return functorList;
  },
  Foldable1: function() {
    return foldableList;
  }
};
var unfoldable1List = {
  unfoldr1: function(f) {
    return function(b) {
      var go = function($copy_source) {
        return function($copy_memo) {
          var $tco_var_source = $copy_source;
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(source, memo) {
            var v = f(source);
            if (v.value1 instanceof Just) {
              $tco_var_source = v.value1.value0;
              $copy_memo = new Cons(v.value0, memo);
              return;
            }
            ;
            if (v.value1 instanceof Nothing) {
              $tco_done = true;
              return foldl2(flip(Cons.create))(Nil.value)(new Cons(v.value0, memo));
            }
            ;
            throw new Error("Failed pattern match at Data.List.Types (line 135, column 22 - line 137, column 61): " + [v.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_source, $copy_memo);
          }
          ;
          return $tco_result;
        };
      };
      return go(b)(Nil.value);
    };
  }
};
var unfoldableList = {
  unfoldr: function(f) {
    return function(b) {
      var go = function($copy_source) {
        return function($copy_memo) {
          var $tco_var_source = $copy_source;
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(source, memo) {
            var v = f(source);
            if (v instanceof Nothing) {
              $tco_done = true;
              return foldl2(flip(Cons.create))(Nil.value)(memo);
            }
            ;
            if (v instanceof Just) {
              $tco_var_source = v.value0.value1;
              $copy_memo = new Cons(v.value0.value0, memo);
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.List.Types (line 142, column 22 - line 144, column 52): " + [v.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_source, $copy_memo);
          }
          ;
          return $tco_result;
        };
      };
      return go(b)(Nil.value);
    };
  },
  Unfoldable10: function() {
    return unfoldable1List;
  }
};

// output/Data.List/index.js
var foldl3 = /* @__PURE__ */ foldl(foldableList);
var tail = function(v) {
  if (v instanceof Nil) {
    return Nothing.value;
  }
  ;
  if (v instanceof Cons) {
    return new Just(v.value1);
  }
  ;
  throw new Error("Failed pattern match at Data.List (line 245, column 1 - line 245, column 43): " + [v.constructor.name]);
};
var reverse = /* @__PURE__ */ function() {
  var go = function($copy_v) {
    return function($copy_v1) {
      var $tco_var_v = $copy_v;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v, v1) {
        if (v1 instanceof Nil) {
          $tco_done = true;
          return v;
        }
        ;
        if (v1 instanceof Cons) {
          $tco_var_v = new Cons(v1.value0, v);
          $copy_v1 = v1.value1;
          return;
        }
        ;
        throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): " + [v.constructor.name, v1.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_v, $copy_v1);
      }
      ;
      return $tco_result;
    };
  };
  return go(Nil.value);
}();
var zipWith = function(f) {
  return function(xs) {
    return function(ys) {
      var go = function($copy_v) {
        return function($copy_v1) {
          return function($copy_v2) {
            var $tco_var_v = $copy_v;
            var $tco_var_v1 = $copy_v1;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(v, v1, v2) {
              if (v instanceof Nil) {
                $tco_done = true;
                return v2;
              }
              ;
              if (v1 instanceof Nil) {
                $tco_done = true;
                return v2;
              }
              ;
              if (v instanceof Cons && v1 instanceof Cons) {
                $tco_var_v = v.value1;
                $tco_var_v1 = v1.value1;
                $copy_v2 = new Cons(f(v.value0)(v1.value0), v2);
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.List (line 779, column 3 - line 779, column 21): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_v2);
            }
            ;
            return $tco_result;
          };
        };
      };
      return reverse(go(xs)(ys)(Nil.value));
    };
  };
};
var length = /* @__PURE__ */ foldl3(function(acc) {
  return function(v) {
    return acc + 1 | 0;
  };
})(0);
var index = function($copy_v) {
  return function($copy_v1) {
    var $tco_var_v = $copy_v;
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v, v1) {
      if (v instanceof Nil) {
        $tco_done = true;
        return Nothing.value;
      }
      ;
      if (v instanceof Cons && v1 === 0) {
        $tco_done = true;
        return new Just(v.value0);
      }
      ;
      if (v instanceof Cons) {
        $tco_var_v = v.value1;
        $copy_v1 = v1 - 1 | 0;
        return;
      }
      ;
      throw new Error("Failed pattern match at Data.List (line 281, column 1 - line 281, column 44): " + [v.constructor.name, v1.constructor.name]);
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($tco_var_v, $copy_v1);
    }
    ;
    return $tco_result;
  };
};
var head = function(v) {
  if (v instanceof Nil) {
    return Nothing.value;
  }
  ;
  if (v instanceof Cons) {
    return new Just(v.value0);
  }
  ;
  throw new Error("Failed pattern match at Data.List (line 230, column 1 - line 230, column 22): " + [v.constructor.name]);
};

// output/Data.Map.Internal/index.js
var Leaf = /* @__PURE__ */ function() {
  function Leaf2() {
  }
  ;
  Leaf2.value = new Leaf2();
  return Leaf2;
}();
var Node = /* @__PURE__ */ function() {
  function Node2(value0, value1, value2, value3, value4, value5) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value2;
    this.value3 = value3;
    this.value4 = value4;
    this.value5 = value5;
  }
  ;
  Node2.create = function(value0) {
    return function(value1) {
      return function(value2) {
        return function(value3) {
          return function(value4) {
            return function(value5) {
              return new Node2(value0, value1, value2, value3, value4, value5);
            };
          };
        };
      };
    };
  };
  return Node2;
}();
var IterLeaf = /* @__PURE__ */ function() {
  function IterLeaf2() {
  }
  ;
  IterLeaf2.value = new IterLeaf2();
  return IterLeaf2;
}();
var IterEmit = /* @__PURE__ */ function() {
  function IterEmit2(value0, value1, value2) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value2;
  }
  ;
  IterEmit2.create = function(value0) {
    return function(value1) {
      return function(value2) {
        return new IterEmit2(value0, value1, value2);
      };
    };
  };
  return IterEmit2;
}();
var IterNode = /* @__PURE__ */ function() {
  function IterNode2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  IterNode2.create = function(value0) {
    return function(value1) {
      return new IterNode2(value0, value1);
    };
  };
  return IterNode2;
}();
var unsafeNode = function(k, v, l, r) {
  if (l instanceof Leaf) {
    if (r instanceof Leaf) {
      return new Node(1, 1, k, v, l, r);
    }
    ;
    if (r instanceof Node) {
      return new Node(1 + r.value0 | 0, 1 + r.value1 | 0, k, v, l, r);
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 702, column 5 - line 706, column 39): " + [r.constructor.name]);
  }
  ;
  if (l instanceof Node) {
    if (r instanceof Leaf) {
      return new Node(1 + l.value0 | 0, 1 + l.value1 | 0, k, v, l, r);
    }
    ;
    if (r instanceof Node) {
      return new Node(1 + function() {
        var $280 = l.value0 > r.value0;
        if ($280) {
          return l.value0;
        }
        ;
        return r.value0;
      }() | 0, (1 + l.value1 | 0) + r.value1 | 0, k, v, l, r);
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 708, column 5 - line 712, column 68): " + [r.constructor.name]);
  }
  ;
  throw new Error("Failed pattern match at Data.Map.Internal (line 700, column 32 - line 712, column 68): " + [l.constructor.name]);
};
var toMapIter = /* @__PURE__ */ function() {
  return flip(IterNode.create)(IterLeaf.value);
}();
var stepWith = function(f) {
  return function(next) {
    return function(done) {
      var go = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
          if (v instanceof IterLeaf) {
            $tco_done = true;
            return done(unit);
          }
          ;
          if (v instanceof IterEmit) {
            $tco_done = true;
            return next(v.value0, v.value1, v.value2);
          }
          ;
          if (v instanceof IterNode) {
            $copy_v = f(v.value1)(v.value0);
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 940, column 8 - line 946, column 20): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }
        ;
        return $tco_result;
      };
      return go;
    };
  };
};
var singleton3 = function(k) {
  return function(v) {
    return new Node(1, 1, k, v, Leaf.value, Leaf.value);
  };
};
var unsafeBalancedNode = /* @__PURE__ */ function() {
  var height = function(v) {
    if (v instanceof Leaf) {
      return 0;
    }
    ;
    if (v instanceof Node) {
      return v.value0;
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 757, column 12 - line 759, column 26): " + [v.constructor.name]);
  };
  var rotateLeft = function(k, v, l, rk, rv, rl, rr) {
    if (rl instanceof Node && rl.value0 > height(rr)) {
      return unsafeNode(rl.value2, rl.value3, unsafeNode(k, v, l, rl.value4), unsafeNode(rk, rv, rl.value5, rr));
    }
    ;
    return unsafeNode(rk, rv, unsafeNode(k, v, l, rl), rr);
  };
  var rotateRight = function(k, v, lk, lv, ll, lr, r) {
    if (lr instanceof Node && height(ll) <= lr.value0) {
      return unsafeNode(lr.value2, lr.value3, unsafeNode(lk, lv, ll, lr.value4), unsafeNode(k, v, lr.value5, r));
    }
    ;
    return unsafeNode(lk, lv, ll, unsafeNode(k, v, lr, r));
  };
  return function(k, v, l, r) {
    if (l instanceof Leaf) {
      if (r instanceof Leaf) {
        return singleton3(k)(v);
      }
      ;
      if (r instanceof Node && r.value0 > 1) {
        return rotateLeft(k, v, l, r.value2, r.value3, r.value4, r.value5);
      }
      ;
      return unsafeNode(k, v, l, r);
    }
    ;
    if (l instanceof Node) {
      if (r instanceof Node) {
        if (r.value0 > (l.value0 + 1 | 0)) {
          return rotateLeft(k, v, l, r.value2, r.value3, r.value4, r.value5);
        }
        ;
        if (l.value0 > (r.value0 + 1 | 0)) {
          return rotateRight(k, v, l.value2, l.value3, l.value4, l.value5, r);
        }
        ;
      }
      ;
      if (r instanceof Leaf && l.value0 > 1) {
        return rotateRight(k, v, l.value2, l.value3, l.value4, l.value5, r);
      }
      ;
      return unsafeNode(k, v, l, r);
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 717, column 40 - line 738, column 34): " + [l.constructor.name]);
  };
}();
var lookup = function(dictOrd) {
  var compare2 = compare(dictOrd);
  return function(k) {
    var go = function($copy_v) {
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v) {
        if (v instanceof Leaf) {
          $tco_done = true;
          return Nothing.value;
        }
        ;
        if (v instanceof Node) {
          var v1 = compare2(k)(v.value2);
          if (v1 instanceof LT) {
            $copy_v = v.value4;
            return;
          }
          ;
          if (v1 instanceof GT) {
            $copy_v = v.value5;
            return;
          }
          ;
          if (v1 instanceof EQ) {
            $tco_done = true;
            return new Just(v.value3);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 283, column 7 - line 286, column 22): " + [v1.constructor.name]);
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 280, column 8 - line 286, column 22): " + [v.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($copy_v);
      }
      ;
      return $tco_result;
    };
    return go;
  };
};
var iterMapL = /* @__PURE__ */ function() {
  var go = function($copy_iter) {
    return function($copy_v) {
      var $tco_var_iter = $copy_iter;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(iter, v) {
        if (v instanceof Leaf) {
          $tco_done = true;
          return iter;
        }
        ;
        if (v instanceof Node) {
          if (v.value5 instanceof Leaf) {
            $tco_var_iter = new IterEmit(v.value2, v.value3, iter);
            $copy_v = v.value4;
            return;
          }
          ;
          $tco_var_iter = new IterEmit(v.value2, v.value3, new IterNode(v.value5, iter));
          $copy_v = v.value4;
          return;
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 951, column 13 - line 958, column 48): " + [v.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_iter, $copy_v);
      }
      ;
      return $tco_result;
    };
  };
  return go;
}();
var stepAscCps = /* @__PURE__ */ stepWith(iterMapL);
var stepUnfoldr = /* @__PURE__ */ function() {
  var step = function(k, v, next) {
    return new Just(new Tuple(new Tuple(k, v), next));
  };
  return stepAscCps(step)(function(v) {
    return Nothing.value;
  });
}();
var toUnfoldable = function(dictUnfoldable) {
  var $784 = unfoldr(dictUnfoldable)(stepUnfoldr);
  return function($785) {
    return $784(toMapIter($785));
  };
};
var insert = function(dictOrd) {
  var compare2 = compare(dictOrd);
  return function(k) {
    return function(v) {
      var go = function(v1) {
        if (v1 instanceof Leaf) {
          return singleton3(k)(v);
        }
        ;
        if (v1 instanceof Node) {
          var v2 = compare2(k)(v1.value2);
          if (v2 instanceof LT) {
            return unsafeBalancedNode(v1.value2, v1.value3, go(v1.value4), v1.value5);
          }
          ;
          if (v2 instanceof GT) {
            return unsafeBalancedNode(v1.value2, v1.value3, v1.value4, go(v1.value5));
          }
          ;
          if (v2 instanceof EQ) {
            return new Node(v1.value0, v1.value1, k, v, v1.value4, v1.value5);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 471, column 7 - line 474, column 35): " + [v2.constructor.name]);
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 468, column 8 - line 474, column 35): " + [v1.constructor.name]);
      };
      return go;
    };
  };
};
var empty2 = /* @__PURE__ */ function() {
  return Leaf.value;
}();

// output/Foreign/index.js
var ast2IsAst = function(x) {
  return x;
};

// output/Data.Array/foreign.js
var replicateFill = function(count, value) {
  if (count < 1) {
    return [];
  }
  var result = new Array(count);
  return result.fill(value);
};
var replicatePolyfill = function(count, value) {
  var result = [];
  var n = 0;
  for (var i = 0; i < count; i++) {
    result[n++] = value;
  }
  return result;
};
var replicateImpl = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
var fromFoldableImpl = function() {
  function Cons2(head2, tail2) {
    this.head = head2;
    this.tail = tail2;
  }
  var emptyList = {};
  function curryCons(head2) {
    return function(tail2) {
      return new Cons2(head2, tail2);
    };
  }
  function listToArray(list) {
    var result = [];
    var count = 0;
    var xs = list;
    while (xs !== emptyList) {
      result[count++] = xs.head;
      xs = xs.tail;
    }
    return result;
  }
  return function(foldr4, xs) {
    return listToArray(foldr4(curryCons)(emptyList)(xs));
  };
}();
var sortByImpl = function() {
  function mergeFromTo(compare2, fromOrdering, xs1, xs2, from2, to) {
    var mid;
    var i;
    var j;
    var k;
    var x;
    var y;
    var c;
    mid = from2 + (to - from2 >> 1);
    if (mid - from2 > 1)
      mergeFromTo(compare2, fromOrdering, xs2, xs1, from2, mid);
    if (to - mid > 1)
      mergeFromTo(compare2, fromOrdering, xs2, xs1, mid, to);
    i = from2;
    j = mid;
    k = from2;
    while (i < mid && j < to) {
      x = xs2[i];
      y = xs2[j];
      c = fromOrdering(compare2(x)(y));
      if (c > 0) {
        xs1[k++] = y;
        ++j;
      } else {
        xs1[k++] = x;
        ++i;
      }
    }
    while (i < mid) {
      xs1[k++] = xs2[i++];
    }
    while (j < to) {
      xs1[k++] = xs2[j++];
    }
  }
  return function(compare2, fromOrdering, xs) {
    var out;
    if (xs.length < 2)
      return xs;
    out = xs.slice(0);
    mergeFromTo(compare2, fromOrdering, out, xs.slice(0), 0, xs.length);
    return out;
  };
}();
var zipWithImpl = function(f, xs, ys) {
  var l = xs.length < ys.length ? xs.length : ys.length;
  var result = new Array(l);
  for (var i = 0; i < l; i++) {
    result[i] = f(xs[i])(ys[i]);
  }
  return result;
};
var anyImpl = function(p, xs) {
  var len = xs.length;
  for (var i = 0; i < len; i++) {
    if (p(xs[i]))
      return true;
  }
  return false;
};

// output/Data.Array.ST/foreign.js
var sortByImpl2 = function() {
  function mergeFromTo(compare2, fromOrdering, xs1, xs2, from2, to) {
    var mid;
    var i;
    var j;
    var k;
    var x;
    var y;
    var c;
    mid = from2 + (to - from2 >> 1);
    if (mid - from2 > 1)
      mergeFromTo(compare2, fromOrdering, xs2, xs1, from2, mid);
    if (to - mid > 1)
      mergeFromTo(compare2, fromOrdering, xs2, xs1, mid, to);
    i = from2;
    j = mid;
    k = from2;
    while (i < mid && j < to) {
      x = xs2[i];
      y = xs2[j];
      c = fromOrdering(compare2(x)(y));
      if (c > 0) {
        xs1[k++] = y;
        ++j;
      } else {
        xs1[k++] = x;
        ++i;
      }
    }
    while (i < mid) {
      xs1[k++] = xs2[i++];
    }
    while (j < to) {
      xs1[k++] = xs2[j++];
    }
  }
  return function(compare2, fromOrdering, xs) {
    if (xs.length < 2)
      return xs;
    mergeFromTo(compare2, fromOrdering, xs, xs.slice(0), 0, xs.length);
    return xs;
  };
}();

// output/Data.Function.Uncurried/foreign.js
var runFn2 = function(fn) {
  return function(a) {
    return function(b) {
      return fn(a, b);
    };
  };
};
var runFn3 = function(fn) {
  return function(a) {
    return function(b) {
      return function(c) {
        return fn(a, b, c);
      };
    };
  };
};

// output/Data.Array/index.js
var intercalate1 = /* @__PURE__ */ intercalate(foldableArray);
var zipWith2 = /* @__PURE__ */ runFn3(zipWithImpl);
var intercalate2 = function(dictMonoid) {
  return intercalate1(dictMonoid);
};
var fromFoldable = function(dictFoldable) {
  return runFn2(fromFoldableImpl)(foldr(dictFoldable));
};
var any2 = /* @__PURE__ */ runFn2(anyImpl);

// output/Debug/foreign.js
var req = typeof module === "undefined" ? void 0 : module.require;
var util = function() {
  try {
    return req === void 0 ? void 0 : req("util");
  } catch (e) {
    return void 0;
  }
}();
function _trace(x, k) {
  if (util !== void 0) {
    console.log(util.inspect(x, { depth: null, colors: true }));
  } else {
    console.log(x);
  }
  return k({});
}
var now = function() {
  var perf;
  if (typeof performance !== "undefined") {
    perf = performance;
  } else if (req) {
    try {
      perf = req("perf_hooks").performance;
    } catch (e) {
    }
  }
  return function() {
    return (perf || Date).now();
  };
}();

// output/Debug/index.js
var trace = function() {
  return function(a) {
    return function(k) {
      return _trace(a, k);
    };
  };
};

// output/Effect.Unsafe/foreign.js
var unsafePerformEffect = function(f) {
  return f();
};

// output/Partial.Unsafe/foreign.js
var _unsafePartial = function(f) {
  return f();
};

// output/Partial/foreign.js
var _crashWith = function(msg) {
  throw new Error(msg);
};

// output/Partial/index.js
var crashWith = function() {
  return _crashWith;
};

// output/Partial.Unsafe/index.js
var crashWith2 = /* @__PURE__ */ crashWith();
var unsafePartial = _unsafePartial;
var unsafeCrashWith = function(msg) {
  return unsafePartial(function() {
    return crashWith2(msg);
  });
};

// output/Util/index.js
var intercalate3 = /* @__PURE__ */ intercalate2(monoidString);
var stateful = function(t) {
  return unsafePerformEffect(function __do() {
    var tref = $$new(t)();
    return {
      get: function(v) {
        return unsafePerformEffect(read(tref));
      },
      set: function(tNew) {
        return unsafePerformEffect(write(tNew)(tref));
      }
    };
  });
};
var bug = function(msg) {
  return unsafeCrashWith(intercalate3("\n")(["", "==[ BUG ]=================================================================", msg, "==========================================================================", ""]));
};
var fromJust2 = function(v) {
  if (v instanceof Just) {
    return v.value0;
  }
  ;
  if (v instanceof Nothing) {
    return bug("fromJust failed");
  }
  ;
  throw new Error("Failed pattern match at Util (line 44, column 1 - line 44, column 36): " + [v.constructor.name]);
};

// output/Evaluate/index.js
var $runtime_lazy = function(name2, moduleName, init) {
  var state2 = 0;
  var val;
  return function(lineNumber) {
    if (state2 === 2)
      return val;
    if (state2 === 1)
      throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state2 = 1;
    val = init();
    state2 = 2;
    return val;
  };
};
var show2 = /* @__PURE__ */ show(showBoolean);
var show1 = /* @__PURE__ */ show(showInt);
var foldr3 = /* @__PURE__ */ foldr(foldableList);
var pure2 = /* @__PURE__ */ pure(applicativeMaybe);
var all2 = /* @__PURE__ */ all(foldableList)(heytingAlgebraBoolean);
var div2 = /* @__PURE__ */ div(euclideanRingInt);
var mod2 = /* @__PURE__ */ mod(euclideanRingInt);
var map4 = /* @__PURE__ */ map(functorArray);
var pure1 = /* @__PURE__ */ pure(applicativeEither);
var insert2 = /* @__PURE__ */ insert(ordString);
var pure22 = /* @__PURE__ */ pure(applicativeLazy);
var bind2 = /* @__PURE__ */ bind(bindEither);
var append1 = /* @__PURE__ */ append(semigroupList);
var lookup2 = /* @__PURE__ */ lookup(ordString);
var HoleError = /* @__PURE__ */ function() {
  function HoleError2() {
  }
  ;
  HoleError2.value = new HoleError2();
  return HoleError2;
}();
var BoundaryError = /* @__PURE__ */ function() {
  function BoundaryError2() {
  }
  ;
  BoundaryError2.value = new BoundaryError2();
  return BoundaryError2;
}();
var FreeVarError = /* @__PURE__ */ function() {
  function FreeVarError2() {
  }
  ;
  FreeVarError2.value = new FreeVarError2();
  return FreeVarError2;
}();
var IntVal = /* @__PURE__ */ function() {
  function IntVal2(value0) {
    this.value0 = value0;
  }
  ;
  IntVal2.create = function(value0) {
    return new IntVal2(value0);
  };
  return IntVal2;
}();
var BoolVal = /* @__PURE__ */ function() {
  function BoolVal2(value0) {
    this.value0 = value0;
  }
  ;
  BoolVal2.create = function(value0) {
    return new BoolVal2(value0);
  };
  return BoolVal2;
}();
var ListVal = /* @__PURE__ */ function() {
  function ListVal2(value0) {
    this.value0 = value0;
  }
  ;
  ListVal2.create = function(value0) {
    return new ListVal2(value0);
  };
  return ListVal2;
}();
var FunVal = /* @__PURE__ */ function() {
  function FunVal2(value0) {
    this.value0 = value0;
  }
  ;
  FunVal2.create = function(value0) {
    return new FunVal2(value0);
  };
  return FunVal2;
}();
var printValue = function(val) {
  if (val instanceof BoolVal) {
    return show2(val.value0);
  }
  ;
  if (val instanceof IntVal) {
    return show1(val.value0);
  }
  ;
  if (val instanceof ListVal) {
    return foldr3(function(x1) {
      return function(xs) {
        return "(cons " + (printValue(x1) + (" " + (xs + ")")));
      };
    })("nil")(val.value0);
  }
  ;
  if (val instanceof FunVal) {
    return "<function>";
  }
  ;
  throw new Error("Failed pattern match at Evaluate (line 151, column 18 - line 155, column 29): " + [val.constructor.name]);
};
var evalConst = function(v) {
  if (v === "true") {
    return pure2(new BoolVal(true));
  }
  ;
  if (v === "false") {
    return pure2(new BoolVal(false));
  }
  ;
  return Nothing.value;
};
var eqValue = function(v1) {
  return function(v2) {
    var v = new Tuple(v1, v2);
    if (v.value0 instanceof IntVal && v.value1 instanceof IntVal) {
      return v.value0.value0 === v.value1.value0;
    }
    ;
    if (v.value0 instanceof BoolVal && v.value1 instanceof BoolVal) {
      return v.value0.value0 === v.value1.value0;
    }
    ;
    if (v.value0 instanceof ListVal && v.value1 instanceof ListVal) {
      return all2(function(x1) {
        return x1;
      })(zipWith(eqValue)(v.value0.value0)(v.value1.value0));
    }
    ;
    return false;
  };
};
var assertValList = function(v) {
  if (v instanceof ListVal) {
    return v.value0;
  }
  ;
  return bug("assertValint failed");
};
var assertValInt = function(v) {
  if (v instanceof IntVal) {
    return v.value0;
  }
  ;
  return bug("assertValint failed");
};
var assertValFun = function(v) {
  if (v instanceof FunVal) {
    return v.value0;
  }
  ;
  return bug("assertValint failed");
};
var assertValBool = function(v) {
  if (v instanceof BoolVal) {
    return v.value0;
  }
  ;
  return bug("assertValint failed");
};
var evalInfix = function(op) {
  if (op === "+") {
    return function(x) {
      return function(y) {
        return new IntVal(assertValInt(x) + assertValInt(y) | 0);
      };
    };
  }
  ;
  if (op === "-") {
    return function(x) {
      return function(y) {
        return new IntVal(assertValInt(x) - assertValInt(y) | 0);
      };
    };
  }
  ;
  if (op === "*") {
    return function(x) {
      return function(y) {
        return new IntVal(assertValInt(x) * assertValInt(y) | 0);
      };
    };
  }
  ;
  if (op === "/") {
    return function(x) {
      return function(y) {
        return new IntVal(div2(assertValInt(x))(assertValInt(y)));
      };
    };
  }
  ;
  if (op === "%") {
    return function(x) {
      return function(y) {
        return new IntVal(mod2(assertValInt(x))(assertValInt(y)));
      };
    };
  }
  ;
  if (op === "^") {
    return function(x) {
      return function(y) {
        return new IntVal(pow(assertValInt(x))(assertValInt(y)));
      };
    };
  }
  ;
  if (op === "<") {
    return function(x) {
      return function(y) {
        return new BoolVal(assertValInt(x) < assertValInt(y));
      };
    };
  }
  ;
  if (op === ">") {
    return function(x) {
      return function(y) {
        return new BoolVal(assertValInt(x) > assertValInt(y));
      };
    };
  }
  ;
  if (op === "<=") {
    return function(x) {
      return function(y) {
        return new BoolVal(assertValInt(x) <= assertValInt(y));
      };
    };
  }
  ;
  if (op === ">=") {
    return function(x) {
      return function(y) {
        return new BoolVal(assertValInt(x) >= assertValInt(y));
      };
    };
  }
  ;
  if (op === "&&") {
    return function(x) {
      return function(y) {
        return new BoolVal(assertValBool(x) && assertValBool(y));
      };
    };
  }
  ;
  if (op === "||") {
    return function(x) {
      return function(y) {
        return new BoolVal(assertValBool(x) || assertValBool(y));
      };
    };
  }
  ;
  return bug("evalInfix case not found: " + op);
};
var $$eval = function(v) {
  return function(v1) {
    var v2 = new Tuple(v1.label, map4(ast2IsAst)(v1.kids));
    if (v2.value0 === "TOP" && v2.value1.length === 1) {
      return $$eval(v)(v2["value1"][0]);
    }
    ;
    if (v2.value0 === "PARENTERM" && v2.value1.length === 1) {
      return $$eval(v)(v2["value1"][0]);
    }
    ;
    if (v2.value0 === "FUN" && v2.value1.length === 2) {
      return pure1(new FunVal(function(x) {
        return $$eval(insert2(v1.dataa)(pure22(new Right(x)))(v))(v2["value1"][1]);
      }));
    }
    ;
    if (v2.value0 === "FUN_NOANN" && v2.value1.length === 1) {
      return pure1(new FunVal(function(x) {
        return $$eval(insert2(v1.dataa)(pure22(new Right(x)))(v))(v2["value1"][0]);
      }));
    }
    ;
    if (v2.value0 === "LET" && v2.value1.length === 3) {
      var $lazy_vDef = $runtime_lazy("vDef", "Evaluate", function() {
        return $$eval(insert2(v1.dataa)(defer2(function(v32) {
          return $lazy_vDef(85);
        }))(v))(v2["value1"][1]);
      });
      var vDef = $lazy_vDef(85);
      return $$eval(insert2(v1.dataa)(pure22(vDef))(v))(v2["value1"][2]);
    }
    ;
    if (v2.value0 === "LET_NOANN" && v2.value1.length === 2) {
      var $lazy_vDef = $runtime_lazy("vDef", "Evaluate", function() {
        return $$eval(insert2(v1.dataa)(defer2(function(v32) {
          return $lazy_vDef(88);
        }))(v))(v2["value1"][0]);
      });
      var vDef = $lazy_vDef(88);
      return $$eval(insert2(v1.dataa)(pure22(vDef))(v))(v2["value1"][1]);
    }
    ;
    if (v2.value0 === "APP" && v2.value1.length === 2) {
      return bind2($$eval(v)(v2["value1"][0]))(function(v11) {
        return bind2($$eval(v)(v2["value1"][1]))(function(v21) {
          return assertValFun(v11)(v21);
        });
      });
    }
    ;
    var v3 = function(v4) {
      if (v2.value0 === "NAME" && (v2.value1.length === 0 && v1.dataa === "nil")) {
        return pure1(new ListVal(Nil.value));
      }
      ;
      if (v2.value0 === "NAME" && (v2.value1.length === 0 && v1.dataa === "cons")) {
        return pure1(new FunVal(function(x) {
          return pure1(new FunVal(function(xs) {
            return pure1(new ListVal(new Cons(x, assertValList(xs))));
          }));
        }));
      }
      ;
      if (v2.value0 === "NAME" && (v2.value1.length === 0 && v1.dataa === "length")) {
        return pure1(new FunVal(function(xs) {
          return pure1(new IntVal(length(assertValList(xs))));
        }));
      }
      ;
      if (v2.value0 === "NAME" && (v2.value1.length === 0 && v1.dataa === "append")) {
        return pure1(new FunVal(function(xs) {
          return pure1(new FunVal(function(ys) {
            return pure1(new ListVal(append1(assertValList(xs))(assertValList(ys))));
          }));
        }));
      }
      ;
      if (v2.value0 === "NAME" && (v2.value1.length === 0 && v1.dataa === "head")) {
        return pure1(new FunVal(function(xs) {
          return pure1(fromJust2(head(assertValList(xs))));
        }));
      }
      ;
      if (v2.value0 === "NAME" && (v2.value1.length === 0 && v1.dataa === "tail")) {
        return pure1(new FunVal(function(xs) {
          return pure1(new ListVal(fromJust2(tail(assertValList(xs)))));
        }));
      }
      ;
      if (v2.value0 === "NAME" && (v2.value1.length === 0 && v1.dataa === "index")) {
        return pure1(new FunVal(function(xs) {
          return pure1(new FunVal(function(n) {
            return pure1(fromJust2(index(assertValList(xs))(assertValInt(n))));
          }));
        }));
      }
      ;
      if (v2.value0 === "NAME" && v2.value1.length === 0) {
        return force(fromJust2(lookup2(v1.dataa)(v)));
      }
      ;
      if (v2.value0 === "HOLE" && v2.value1.length === 0) {
        return new Left(HoleError.value);
      }
      ;
      if (v2.value0 === "IF" && v2.value1.length === 3) {
        return bind2($$eval(v)(v2["value1"][0]))(function(vCond) {
          var $122 = assertValBool(vCond);
          if ($122) {
            return $$eval(v)(v2["value1"][1]);
          }
          ;
          return $$eval(v)(v2["value1"][2]);
        });
      }
      ;
      if (v2.value0 === "INFIX_EXPRESSION" && v2.value1.length === 2) {
        return bind2($$eval(v)(v2["value1"][0]))(function(v11) {
          return bind2($$eval(v)(v2["value1"][1]))(function(v21) {
            return pure1(evalInfix(v1.dataa)(v11)(v21));
          });
        });
      }
      ;
      if (v2.value0 === "EQUALS" && v2.value1.length === 2) {
        return bind2($$eval(v)(v2["value1"][0]))(function(v11) {
          return bind2($$eval(v)(v2["value1"][1]))(function(v21) {
            return pure1(new BoolVal(eqValue(v11)(v21)));
          });
        });
      }
      ;
      if (v2.value0 === "MATCH" && v2.value1.length === 3) {
        return bind2($$eval(v)(v2["value1"][0]))(function(vLi) {
          var v5 = assertValList(vLi);
          if (v5 instanceof Nil) {
            return $$eval(v)(v2["value1"][1]);
          }
          ;
          if (v5 instanceof Cons) {
            return $$eval(insert2(v1.dataa)(pure22(new Right(v5.value0)))(insert2(v1.dataa2)(pure22(new Right(new ListVal(v5.value1))))(v)))(v2["value1"][2]);
          }
          ;
          throw new Error("Failed pattern match at Evaluate (line 119, column 13 - line 121, column 128): " + [v5.constructor.name]);
        });
      }
      ;
      if (v2.value0 === "INTEGER") {
        return pure1(new IntVal(fromJust2(fromString(v1.dataa))));
      }
      ;
      if (v2.value0 === "NOT" && v2.value1.length === 1) {
        return pure1(new FunVal(function(b1) {
          return pure1(new BoolVal(!assertValBool(b1)));
        }));
      }
      ;
      return bug("eval case fail: label was " + v1.label);
    };
    if (v2.value0 === "NAME" && v2.value1.length === 0) {
      var $150 = evalConst(v1.dataa);
      if ($150 instanceof Just) {
        return pure1($150.value0);
      }
      ;
      return v3(true);
    }
    ;
    return v3(true);
  };
};
var evaluate = function(dterm) {
  var v = $$eval(empty2)(dterm);
  if (v instanceof Left) {
    if (v.value0 instanceof HoleError) {
      return "Error: hole";
    }
    ;
    if (v.value0 instanceof BoundaryError) {
      return "Error: type boundary";
    }
    ;
    if (v.value0 instanceof FreeVarError) {
      return "Error: unbound variable";
    }
    ;
    throw new Error("Failed pattern match at Evaluate (line 31, column 19 - line 34, column 50): " + [v.value0.constructor.name]);
  }
  ;
  if (v instanceof Right) {
    return printValue(v.value0);
  }
  ;
  throw new Error("Failed pattern match at Evaluate (line 30, column 18 - line 35, column 32): " + [v.constructor.name]);
};

// output/Data.Eq.Generic/index.js
var genericEqNoArguments = {
  "genericEq'": function(v) {
    return function(v1) {
      return true;
    };
  }
};
var genericEqArgument = function(dictEq) {
  var eq3 = eq(dictEq);
  return {
    "genericEq'": function(v) {
      return function(v1) {
        return eq3(v)(v1);
      };
    }
  };
};
var genericEq$prime = function(dict) {
  return dict["genericEq'"];
};
var genericEqConstructor = function(dictGenericEq) {
  var genericEq$prime1 = genericEq$prime(dictGenericEq);
  return {
    "genericEq'": function(v) {
      return function(v1) {
        return genericEq$prime1(v)(v1);
      };
    }
  };
};
var genericEqProduct = function(dictGenericEq) {
  var genericEq$prime1 = genericEq$prime(dictGenericEq);
  return function(dictGenericEq1) {
    var genericEq$prime2 = genericEq$prime(dictGenericEq1);
    return {
      "genericEq'": function(v) {
        return function(v1) {
          return genericEq$prime1(v.value0)(v1.value0) && genericEq$prime2(v.value1)(v1.value1);
        };
      }
    };
  };
};
var genericEqSum = function(dictGenericEq) {
  var genericEq$prime1 = genericEq$prime(dictGenericEq);
  return function(dictGenericEq1) {
    var genericEq$prime2 = genericEq$prime(dictGenericEq1);
    return {
      "genericEq'": function(v) {
        return function(v1) {
          if (v instanceof Inl && v1 instanceof Inl) {
            return genericEq$prime1(v.value0)(v1.value0);
          }
          ;
          if (v instanceof Inr && v1 instanceof Inr) {
            return genericEq$prime2(v.value0)(v1.value0);
          }
          ;
          return false;
        };
      }
    };
  };
};
var genericEq = function(dictGeneric) {
  var from2 = from(dictGeneric);
  return function(dictGenericEq) {
    var genericEq$prime1 = genericEq$prime(dictGenericEq);
    return function(x) {
      return function(y) {
        return genericEq$prime1(from2(x))(from2(y));
      };
    };
  };
};

// output/Data.Ord.Generic/index.js
var genericOrdNoArguments = {
  "genericCompare'": function(v) {
    return function(v1) {
      return EQ.value;
    };
  }
};
var genericOrdArgument = function(dictOrd) {
  var compare2 = compare(dictOrd);
  return {
    "genericCompare'": function(v) {
      return function(v1) {
        return compare2(v)(v1);
      };
    }
  };
};
var genericCompare$prime = function(dict) {
  return dict["genericCompare'"];
};
var genericOrdConstructor = function(dictGenericOrd) {
  var genericCompare$prime1 = genericCompare$prime(dictGenericOrd);
  return {
    "genericCompare'": function(v) {
      return function(v1) {
        return genericCompare$prime1(v)(v1);
      };
    }
  };
};
var genericOrdProduct = function(dictGenericOrd) {
  var genericCompare$prime1 = genericCompare$prime(dictGenericOrd);
  return function(dictGenericOrd1) {
    var genericCompare$prime2 = genericCompare$prime(dictGenericOrd1);
    return {
      "genericCompare'": function(v) {
        return function(v1) {
          var v2 = genericCompare$prime1(v.value0)(v1.value0);
          if (v2 instanceof EQ) {
            return genericCompare$prime2(v.value1)(v1.value1);
          }
          ;
          return v2;
        };
      }
    };
  };
};
var genericOrdSum = function(dictGenericOrd) {
  var genericCompare$prime1 = genericCompare$prime(dictGenericOrd);
  return function(dictGenericOrd1) {
    var genericCompare$prime2 = genericCompare$prime(dictGenericOrd1);
    return {
      "genericCompare'": function(v) {
        return function(v1) {
          if (v instanceof Inl && v1 instanceof Inl) {
            return genericCompare$prime1(v.value0)(v1.value0);
          }
          ;
          if (v instanceof Inr && v1 instanceof Inr) {
            return genericCompare$prime2(v.value0)(v1.value0);
          }
          ;
          if (v instanceof Inl && v1 instanceof Inr) {
            return LT.value;
          }
          ;
          if (v instanceof Inr && v1 instanceof Inl) {
            return GT.value;
          }
          ;
          throw new Error("Failed pattern match at Data.Ord.Generic (line 19, column 1 - line 23, column 39): " + [v.constructor.name, v1.constructor.name]);
        };
      }
    };
  };
};
var genericCompare = function(dictGeneric) {
  var from2 = from(dictGeneric);
  return function(dictGenericOrd) {
    var genericCompare$prime1 = genericCompare$prime(dictGenericOrd);
    return function(x) {
      return function(y) {
        return genericCompare$prime1(from2(x))(from2(y));
      };
    };
  };
};

// output/Data.Show.Generic/foreign.js
var intercalate4 = function(separator) {
  return function(xs) {
    return xs.join(separator);
  };
};

// output/Data.Show.Generic/index.js
var append2 = /* @__PURE__ */ append(semigroupArray);
var genericShowArgsNoArguments = {
  genericShowArgs: function(v) {
    return [];
  }
};
var genericShowArgsArgument = function(dictShow) {
  var show5 = show(dictShow);
  return {
    genericShowArgs: function(v) {
      return [show5(v)];
    }
  };
};
var genericShowArgs = function(dict) {
  return dict.genericShowArgs;
};
var genericShowConstructor = function(dictGenericShowArgs) {
  var genericShowArgs1 = genericShowArgs(dictGenericShowArgs);
  return function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return {
      "genericShow'": function(v) {
        var ctor = reflectSymbol2($$Proxy.value);
        var v1 = genericShowArgs1(v);
        if (v1.length === 0) {
          return ctor;
        }
        ;
        return "(" + (intercalate4(" ")(append2([ctor])(v1)) + ")");
      }
    };
  };
};
var genericShow$prime = function(dict) {
  return dict["genericShow'"];
};
var genericShowSum = function(dictGenericShow) {
  var genericShow$prime1 = genericShow$prime(dictGenericShow);
  return function(dictGenericShow1) {
    var genericShow$prime2 = genericShow$prime(dictGenericShow1);
    return {
      "genericShow'": function(v) {
        if (v instanceof Inl) {
          return genericShow$prime1(v.value0);
        }
        ;
        if (v instanceof Inr) {
          return genericShow$prime2(v.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Show.Generic (line 26, column 1 - line 28, column 40): " + [v.constructor.name]);
      }
    };
  };
};
var genericShow = function(dictGeneric) {
  var from2 = from(dictGeneric);
  return function(dictGenericShow) {
    var genericShow$prime1 = genericShow$prime(dictGenericShow);
    return function(x) {
      return genericShow$prime1(from2(x));
    };
  };
};

// output/Text.Pretty/index.js
var pretty = function(dict) {
  return dict.pretty;
};

// output/Data.Expr/index.js
var show3 = /* @__PURE__ */ show(showInt);
var map5 = /* @__PURE__ */ map(functorArray);
var MetaVar = /* @__PURE__ */ function() {
  function MetaVar2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  MetaVar2.create = function(value0) {
    return function(value1) {
      return new MetaVar2(value0, value1);
    };
  };
  return MetaVar2;
}();
var RuleMetaVar = /* @__PURE__ */ function() {
  function RuleMetaVar2(value0) {
    this.value0 = value0;
  }
  ;
  RuleMetaVar2.create = function(value0) {
    return new RuleMetaVar2(value0);
  };
  return RuleMetaVar2;
}();
var MV = /* @__PURE__ */ function() {
  function MV2(value0) {
    this.value0 = value0;
  }
  ;
  MV2.create = function(value0) {
    return new MV2(value0);
  };
  return MV2;
}();
var MInj = /* @__PURE__ */ function() {
  function MInj2(value0) {
    this.value0 = value0;
  }
  ;
  MInj2.create = function(value0) {
    return new MInj2(value0);
  };
  return MInj2;
}();
var Expr = /* @__PURE__ */ function() {
  function Expr2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Expr2.create = function(value0) {
    return function(value1) {
      return new Expr2(value0, value1);
    };
  };
  return Expr2;
}();
var prettyMetaVar = {
  pretty: function(v) {
    if (v instanceof MetaVar && v.value0 instanceof Nothing) {
      return "?" + show3(v.value1);
    }
    ;
    if (v instanceof MetaVar && v.value0 instanceof Just) {
      return "?" + (v.value0.value0 + ("#" + show3(v.value1)));
    }
    ;
    if (v instanceof RuleMetaVar) {
      return "??" + v.value0;
    }
    ;
    throw new Error("Failed pattern match at Data.Expr (line 97, column 1 - line 100, column 41): " + [v.constructor.name]);
  }
};
var genericMetaVar_ = {
  to: function(x) {
    if (x instanceof Inl) {
      return new MetaVar(x.value0.value0, x.value0.value1);
    }
    ;
    if (x instanceof Inr) {
      return new RuleMetaVar(x.value0);
    }
    ;
    throw new Error("Failed pattern match at Data.Expr (line 93, column 1 - line 93, column 34): " + [x.constructor.name]);
  },
  from: function(x) {
    if (x instanceof MetaVar) {
      return new Inl(new Product(x.value0, x.value1));
    }
    ;
    if (x instanceof RuleMetaVar) {
      return new Inr(x.value0);
    }
    ;
    throw new Error("Failed pattern match at Data.Expr (line 93, column 1 - line 93, column 34): " + [x.constructor.name]);
  }
};
var genericEq1 = /* @__PURE__ */ genericEq(genericMetaVar_)(/* @__PURE__ */ genericEqSum(/* @__PURE__ */ genericEqConstructor(/* @__PURE__ */ genericEqProduct(/* @__PURE__ */ genericEqArgument(/* @__PURE__ */ eqMaybe(eqString)))(/* @__PURE__ */ genericEqArgument(eqInt))))(/* @__PURE__ */ genericEqConstructor(/* @__PURE__ */ genericEqArgument(eqString))));
var genericCompare1 = /* @__PURE__ */ genericCompare(genericMetaVar_)(/* @__PURE__ */ genericOrdSum(/* @__PURE__ */ genericOrdConstructor(/* @__PURE__ */ genericOrdProduct(/* @__PURE__ */ genericOrdArgument(/* @__PURE__ */ ordMaybe(ordString)))(/* @__PURE__ */ genericOrdArgument(ordInt))))(/* @__PURE__ */ genericOrdConstructor(/* @__PURE__ */ genericOrdArgument(ordString))));
var eqMetaVar = {
  eq: function(x) {
    return function(y) {
      return genericEq1(x)(y);
    };
  }
};
var ordMetaVar = {
  compare: function(x) {
    return function(y) {
      return genericCompare1(x)(y);
    };
  },
  Eq0: function() {
    return eqMetaVar;
  }
};
var lookup3 = /* @__PURE__ */ lookup(ordMetaVar);
var varMaker = /* @__PURE__ */ stateful(0);
var subMetaExprPartially = function(dictIsExprLabel) {
  return function(sigma) {
    return function(v) {
      if (v.value0 instanceof MV) {
        var v1 = lookup3(v.value0.value0)(sigma);
        if (v1 instanceof Nothing) {
          return new Expr(new MV(v.value0.value0), []);
        }
        ;
        if (v1 instanceof Just) {
          return v1.value0;
        }
        ;
        throw new Error("Failed pattern match at Data.Expr (line 172, column 18 - line 174, column 26): " + [v1.constructor.name]);
      }
      ;
      if (v.value0 instanceof MInj) {
        return new Expr(new MInj(v.value0.value0), map5(subMetaExprPartially(dictIsExprLabel)(sigma))(v.value1));
      }
      ;
      throw new Error("Failed pattern match at Data.Expr (line 171, column 30 - line 175, column 70): " + [v.constructor.name]);
    };
  };
};
var pureMetaExpr = function(l) {
  return function(v) {
    return new Expr(new MInj(l), v);
  };
};
var nextInt = function(v) {
  var val = varMaker.get(unit);
  var v1 = varMaker.set(val + 1 | 0);
  return val;
};
var fromMetaVar = function(mx) {
  return new Expr(new MV(mx), []);
};
var freshMetaVar = function(str) {
  return new MetaVar(new Just(str), nextInt(unit));
};

// output/Data.String.Common/foreign.js
var joinWith = function(s) {
  return function(xs) {
    return xs.join(s);
  };
};

// output/Data.String.Common/index.js
var $$null = function(s) {
  return s === "";
};

// output/Grammar/index.js
var map6 = /* @__PURE__ */ map(functorArray);
var Rule = /* @__PURE__ */ function() {
  function Rule2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Rule2.create = function(value0) {
    return function(value1) {
      return new Rule2(value0, value1);
    };
  };
  return Rule2;
}();
var makeRule$prime = function(strs) {
  return function(f) {
    var mxs = map6(freshMetaVar)(strs);
    var es = map6(fromMetaVar)(mxs);
    var v = f(es);
    return new Rule(v.value0, v.value1);
  };
};
var makeRule = function(strs) {
  return function(f) {
    var f1 = f();
    return makeRule$prime(strs)(f1);
  };
};
var freshMetaVarSort = function(name2) {
  return new Expr(new MV(freshMetaVar(name2)), []);
};

// output/Language/index.js
var genericShowConstructor2 = /* @__PURE__ */ genericShowConstructor(genericShowArgsNoArguments);
var genericEqConstructor2 = /* @__PURE__ */ genericEqConstructor(genericEqNoArguments);
var genericEqSum2 = /* @__PURE__ */ genericEqSum(genericEqConstructor2);
var genericEqSum1 = /* @__PURE__ */ genericEqSum2(genericEqConstructor2);
var genericOrdConstructor2 = /* @__PURE__ */ genericOrdConstructor(genericOrdNoArguments);
var genericOrdSum2 = /* @__PURE__ */ genericOrdSum(genericOrdConstructor2);
var genericOrdSum1 = /* @__PURE__ */ genericOrdSum2(genericOrdConstructor2);
var pure3 = /* @__PURE__ */ pure(applicativeMaybe);
var map7 = /* @__PURE__ */ map(functorArray);
var pretty2 = /* @__PURE__ */ pretty(prettyMetaVar);
var pure12 = /* @__PURE__ */ pure(applicativeEither);
var Bool = /* @__PURE__ */ function() {
  function Bool2() {
  }
  ;
  Bool2.value = new Bool2();
  return Bool2;
}();
var Int = /* @__PURE__ */ function() {
  function Int2() {
  }
  ;
  Int2.value = new Int2();
  return Int2;
}();
var TermSort = /* @__PURE__ */ function() {
  function TermSort2() {
  }
  ;
  TermSort2.value = new TermSort2();
  return TermSort2;
}();
var TypeSort = /* @__PURE__ */ function() {
  function TypeSort2() {
  }
  ;
  TypeSort2.value = new TypeSort2();
  return TypeSort2;
}();
var CtxConsSort = /* @__PURE__ */ function() {
  function CtxConsSort2(value0) {
    this.value0 = value0;
  }
  ;
  CtxConsSort2.create = function(value0) {
    return new CtxConsSort2(value0);
  };
  return CtxConsSort2;
}();
var CtxNilSort = /* @__PURE__ */ function() {
  function CtxNilSort2() {
  }
  ;
  CtxNilSort2.value = new CtxNilSort2();
  return CtxNilSort2;
}();
var DataType = /* @__PURE__ */ function() {
  function DataType2(value0) {
    this.value0 = value0;
  }
  ;
  DataType2.create = function(value0) {
    return new DataType2(value0);
  };
  return DataType2;
}();
var Arrow = /* @__PURE__ */ function() {
  function Arrow2() {
  }
  ;
  Arrow2.value = new Arrow2();
  return Arrow2;
}();
var List = /* @__PURE__ */ function() {
  function List2() {
  }
  ;
  List2.value = new List2();
  return List2;
}();
var UnificationError = /* @__PURE__ */ function() {
  function UnificationError2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  UnificationError2.create = function(value0) {
    return function(value1) {
      return new UnificationError2(value0, value1);
    };
  };
  return UnificationError2;
}();
var HoleMessage = /* @__PURE__ */ function() {
  function HoleMessage2(value0) {
    this.value0 = value0;
  }
  ;
  HoleMessage2.create = function(value0) {
    return new HoleMessage2(value0);
  };
  return HoleMessage2;
}();
var UnboundVariable = /* @__PURE__ */ function() {
  function UnboundVariable2(value0) {
    this.value0 = value0;
  }
  ;
  UnboundVariable2.create = function(value0) {
    return new UnboundVariable2(value0);
  };
  return UnboundVariable2;
}();
var genericSortLabel_ = {
  to: function(x) {
    if (x instanceof Inl) {
      return TermSort.value;
    }
    ;
    if (x instanceof Inr && x.value0 instanceof Inl) {
      return TypeSort.value;
    }
    ;
    if (x instanceof Inr && (x.value0 instanceof Inr && x.value0.value0 instanceof Inl)) {
      return new CtxConsSort(x.value0.value0.value0);
    }
    ;
    if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && x.value0.value0.value0 instanceof Inl))) {
      return CtxNilSort.value;
    }
    ;
    if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0 instanceof Inl)))) {
      return new DataType(x.value0.value0.value0.value0.value0);
    }
    ;
    if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0 instanceof Inl))))) {
      return Arrow.value;
    }
    ;
    if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0 instanceof Inr))))) {
      return List.value;
    }
    ;
    throw new Error("Failed pattern match at Language (line 69, column 1 - line 69, column 36): " + [x.constructor.name]);
  },
  from: function(x) {
    if (x instanceof TermSort) {
      return new Inl(NoArguments.value);
    }
    ;
    if (x instanceof TypeSort) {
      return new Inr(new Inl(NoArguments.value));
    }
    ;
    if (x instanceof CtxConsSort) {
      return new Inr(new Inr(new Inl(x.value0)));
    }
    ;
    if (x instanceof CtxNilSort) {
      return new Inr(new Inr(new Inr(new Inl(NoArguments.value))));
    }
    ;
    if (x instanceof DataType) {
      return new Inr(new Inr(new Inr(new Inr(new Inl(x.value0)))));
    }
    ;
    if (x instanceof Arrow) {
      return new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value))))));
    }
    ;
    if (x instanceof List) {
      return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(NoArguments.value))))));
    }
    ;
    throw new Error("Failed pattern match at Language (line 69, column 1 - line 69, column 36): " + [x.constructor.name]);
  }
};
var genericDataType_ = {
  to: function(x) {
    if (x instanceof Inl) {
      return Bool.value;
    }
    ;
    if (x instanceof Inr) {
      return Int.value;
    }
    ;
    throw new Error("Failed pattern match at Language (line 25, column 1 - line 25, column 35): " + [x.constructor.name]);
  },
  from: function(x) {
    if (x instanceof Bool) {
      return new Inl(NoArguments.value);
    }
    ;
    if (x instanceof Int) {
      return new Inr(NoArguments.value);
    }
    ;
    throw new Error("Failed pattern match at Language (line 25, column 1 - line 25, column 35): " + [x.constructor.name]);
  }
};
var genericShow2 = /* @__PURE__ */ genericShow(genericDataType_)(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
  reflectSymbol: function() {
    return "Bool";
  }
}))(/* @__PURE__ */ genericShowConstructor2({
  reflectSymbol: function() {
    return "Int";
  }
})));
var genericEq2 = /* @__PURE__ */ genericEq(genericDataType_)(genericEqSum1);
var genericCompare2 = /* @__PURE__ */ genericCompare(genericDataType_)(genericOrdSum1);
var showDataType = {
  show: function(x) {
    return genericShow2(x);
  }
};
var genericShow1 = /* @__PURE__ */ genericShow(genericSortLabel_)(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
  reflectSymbol: function() {
    return "TermSort";
  }
}))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
  reflectSymbol: function() {
    return "TypeSort";
  }
}))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor(/* @__PURE__ */ genericShowArgsArgument(showString))({
  reflectSymbol: function() {
    return "CtxConsSort";
  }
}))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
  reflectSymbol: function() {
    return "CtxNilSort";
  }
}))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor(/* @__PURE__ */ genericShowArgsArgument(showDataType))({
  reflectSymbol: function() {
    return "DataType";
  }
}))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
  reflectSymbol: function() {
    return "Arrow";
  }
}))(/* @__PURE__ */ genericShowConstructor2({
  reflectSymbol: function() {
    return "List";
  }
}))))))));
var show4 = /* @__PURE__ */ show(showDataType);
var showSortLabel = {
  show: function(x) {
    return genericShow1(x);
  }
};
var eqDataType = {
  eq: function(x) {
    return genericEq2(x);
  }
};
var genericEq12 = /* @__PURE__ */ genericEq(genericSortLabel_)(/* @__PURE__ */ genericEqSum2(/* @__PURE__ */ genericEqSum2(/* @__PURE__ */ genericEqSum(/* @__PURE__ */ genericEqConstructor(/* @__PURE__ */ genericEqArgument(eqString)))(/* @__PURE__ */ genericEqSum2(/* @__PURE__ */ genericEqSum(/* @__PURE__ */ genericEqConstructor(/* @__PURE__ */ genericEqArgument(eqDataType)))(genericEqSum1))))));
var eqSortLabel = {
  eq: function(x) {
    return genericEq12(x);
  }
};
var ordDataType = {
  compare: function(x) {
    return function(y) {
      return genericCompare2(x)(y);
    };
  },
  Eq0: function() {
    return eqDataType;
  }
};
var genericCompare12 = /* @__PURE__ */ genericCompare(genericSortLabel_)(/* @__PURE__ */ genericOrdSum2(/* @__PURE__ */ genericOrdSum2(/* @__PURE__ */ genericOrdSum(/* @__PURE__ */ genericOrdConstructor(/* @__PURE__ */ genericOrdArgument(ordString)))(/* @__PURE__ */ genericOrdSum2(/* @__PURE__ */ genericOrdSum(/* @__PURE__ */ genericOrdConstructor(/* @__PURE__ */ genericOrdArgument(ordDataType)))(genericOrdSum1))))));
var ordSortLabel = {
  compare: function(x) {
    return function(y) {
      return genericCompare12(x)(y);
    };
  },
  Eq0: function() {
    return eqSortLabel;
  }
};
var isExprLabelSortLabel = {
  Eq0: function() {
    return eqSortLabel;
  },
  Ord1: function() {
    return ordSortLabel;
  },
  Show2: function() {
    return showSortLabel;
  }
};
var surround = function(left) {
  return function(right) {
    return function(str) {
      return left + (str + right);
    };
  };
};
var parens = /* @__PURE__ */ surround("(")(")");
var infixTypes = function(op) {
  var $$int = pureMetaExpr(new DataType(Int.value))([]);
  var bool = pureMetaExpr(new DataType(Bool.value))([]);
  if (op === "+") {
    return {
      left: $$int,
      right: $$int,
      output: $$int
    };
  }
  ;
  if (op === "-") {
    return {
      left: $$int,
      right: $$int,
      output: $$int
    };
  }
  ;
  if (op === "*") {
    return {
      left: $$int,
      right: $$int,
      output: $$int
    };
  }
  ;
  if (op === "/") {
    return {
      left: $$int,
      right: $$int,
      output: $$int
    };
  }
  ;
  if (op === "%") {
    return {
      left: $$int,
      right: $$int,
      output: $$int
    };
  }
  ;
  if (op === "^") {
    return {
      left: $$int,
      right: $$int,
      output: $$int
    };
  }
  ;
  if (op === "<") {
    return {
      left: $$int,
      right: $$int,
      output: bool
    };
  }
  ;
  if (op === ">") {
    return {
      left: $$int,
      right: $$int,
      output: bool
    };
  }
  ;
  if (op === "<=") {
    return {
      left: $$int,
      right: $$int,
      output: bool
    };
  }
  ;
  if (op === ">=") {
    return {
      left: $$int,
      right: $$int,
      output: bool
    };
  }
  ;
  if (op === "&&") {
    return {
      left: bool,
      right: bool,
      output: bool
    };
  }
  ;
  if (op === "||") {
    return {
      left: bool,
      right: bool,
      output: bool
    };
  }
  ;
  return bug("infixTypes error");
};
var constantType = function(v) {
  if (v === "true") {
    return pure3(pureMetaExpr(new DataType(Bool.value))([]));
  }
  ;
  if (v === "false") {
    return pure3(pureMetaExpr(new DataType(Bool.value))([]));
  }
  ;
  return Nothing.value;
};
var appendSpaced = function(v) {
  return function(v1) {
    if ($$null(v)) {
      return v1;
    }
    ;
    if ($$null(v1)) {
      return v;
    }
    ;
    return v + (" " + v1);
  };
};
var printSortImpl = function(v) {
  if (v.value0 instanceof TermSort && v.value1.length === 2) {
    return appendSpaced("Term")(appendSpaced(parens(v["value1"][0]))(v["value1"][1]));
  }
  ;
  if (v.value0 instanceof TypeSort && v.value1.length === 1) {
    return appendSpaced("Type")(parens(v["value1"][0]));
  }
  ;
  if (v.value0 instanceof CtxConsSort && v.value1.length === 2) {
    return v.value0.value0 + (":" + (v["value1"][0] + (", " + v["value1"][1])));
  }
  ;
  if (v.value0 instanceof CtxNilSort && v.value1.length === 0) {
    return "\u2205";
  }
  ;
  if (v.value0 instanceof DataType && v.value1.length === 0) {
    return show4(v.value0.value0);
  }
  ;
  if (v.value0 instanceof Arrow && v.value1.length === 2) {
    return parens(v["value1"][0] + (" -> " + v["value1"][1]));
  }
  ;
  if (v.value0 instanceof List && v.value1.length === 1) {
    return appendSpaced("List")(v["value1"][0]);
  }
  ;
  return bug("printSortImpl");
};
var printSort = function(v) {
  if (v.value0 instanceof MInj) {
    return printSortImpl(new Tuple(v.value0.value0, map7(printSort)(v.value1)));
  }
  ;
  if (v.value0 instanceof MV) {
    return pretty2(v.value0.value0);
  }
  ;
  throw new Error("Failed pattern match at Language (line 65, column 1 - line 65, column 28): " + [v.constructor.name]);
};
var findInCtx = function(sort) {
  return function(name2) {
    var impl = function($copy_v) {
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v) {
        if (v.value0 instanceof MInj && (v.value0.value0 instanceof CtxConsSort && v.value1.length === 2)) {
          var $211 = name2 === v.value0.value0.value0;
          if ($211) {
            $tco_done = true;
            return pure3(v["value1"][0]);
          }
          ;
          $copy_v = v["value1"][1];
          return;
        }
        ;
        $tco_done = true;
        return Nothing.value;
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($copy_v);
      }
      ;
      return $tco_result;
    };
    if (sort.value0 instanceof MInj && (sort.value0.value0 instanceof TermSort && sort.value1.length === 2)) {
      return impl(sort["value1"][0]);
    }
    ;
    return bug("bug in findInCtx: sort not right " + printSort(sort));
  };
};
var language = function(partialSort) {
  return function(label) {
    return function(dataa) {
      return function(dataa2) {
        if (label === "TOP") {
          return pure12(makeRule(["x"])(function() {
            return function(v2) {
              if (v2.length === 1) {
                return new Tuple([v2[0]], v2[0]);
              }
              ;
              throw new Error("Failed pattern match at Language (line 139, column 42 - line 142, column 6): " + [v2.constructor.name]);
            };
          }));
        }
        ;
        if (label === "PARENTERM") {
          return pure12(makeRule(["x"])(function() {
            return function(v2) {
              if (v2.length === 1) {
                return new Tuple([v2[0]], v2[0]);
              }
              ;
              throw new Error("Failed pattern match at Language (line 144, column 48 - line 147, column 6): " + [v2.constructor.name]);
            };
          }));
        }
        ;
        if (label === "PARENTYPE") {
          return pure12(makeRule(["x"])(function() {
            return function(v2) {
              if (v2.length === 1) {
                return new Tuple([v2[0]], v2[0]);
              }
              ;
              throw new Error("Failed pattern match at Language (line 149, column 48 - line 152, column 6): " + [v2.constructor.name]);
            };
          }));
        }
        ;
        if (label === "FUN") {
          return pure12(makeRule(["gamma", "a", "b"])(function() {
            return function(v2) {
              if (v2.length === 3) {
                return new Tuple([pureMetaExpr(TypeSort.value)([v2[1]]), pureMetaExpr(TermSort.value)([pureMetaExpr(new CtxConsSort(dataa))([v2[1], v2[0]]), v2[2]])], pureMetaExpr(TermSort.value)([v2[0], pureMetaExpr(Arrow.value)([v2[1], v2[2]])]));
              }
              ;
              throw new Error("Failed pattern match at Language (line 154, column 56 - line 158, column 44): " + [v2.constructor.name]);
            };
          }));
        }
        ;
        if (label === "FUN_NOANN") {
          return pure12(makeRule(["gamma", "a", "b"])(function() {
            return function(v2) {
              if (v2.length === 3) {
                return new Tuple([pureMetaExpr(TermSort.value)([pureMetaExpr(new CtxConsSort(dataa))([v2[1], v2[0]]), v2[2]])], pureMetaExpr(TermSort.value)([v2[0], pureMetaExpr(Arrow.value)([v2[1], v2[2]])]));
              }
              ;
              throw new Error("Failed pattern match at Language (line 160, column 62 - line 163, column 44): " + [v2.constructor.name]);
            };
          }));
        }
        ;
        if (label === "IF") {
          return pure12(makeRule(["gamma", "ty"])(function() {
            return function(v2) {
              if (v2.length === 2) {
                return new Tuple([pureMetaExpr(TermSort.value)([v2[0], pureMetaExpr(new DataType(Bool.value))([])]), pureMetaExpr(TermSort.value)([v2[0], v2[1]]), pureMetaExpr(TermSort.value)([v2[0], v2[1]])], pureMetaExpr(TermSort.value)([v2[0], v2[1]]));
              }
              ;
              throw new Error("Failed pattern match at Language (line 165, column 51 - line 170, column 32): " + [v2.constructor.name]);
            };
          }));
        }
        ;
        if (label === "APP") {
          return pure12(makeRule(["gamma", "a", "b"])(function() {
            return function(v2) {
              if (v2.length === 3) {
                return new Tuple([pureMetaExpr(TermSort.value)([v2[0], pureMetaExpr(Arrow.value)([v2[1], v2[2]])]), pureMetaExpr(TermSort.value)([v2[0], v2[1]])], pureMetaExpr(TermSort.value)([v2[0], v2[2]]));
              }
              ;
              throw new Error("Failed pattern match at Language (line 172, column 56 - line 176, column 31): " + [v2.constructor.name]);
            };
          }));
        }
        ;
        if (label === "HOLE") {
          return pure12(makeRule(["gamma", "type"])(function() {
            return function(v2) {
              if (v2.length === 2) {
                return new Tuple([pureMetaExpr(TypeSort.value)([v2[1]])], pureMetaExpr(TermSort.value)([v2[0], v2[1]]));
              }
              ;
              throw new Error("Failed pattern match at Language (line 178, column 55 - line 181, column 32): " + [v2.constructor.name]);
            };
          }));
        }
        ;
        if (label === "LET") {
          return pure12(makeRule(["a", "b", "gamma"])(function() {
            return function(v2) {
              if (v2.length === 3) {
                return new Tuple([pureMetaExpr(TypeSort.value)([v2[0]]), pureMetaExpr(TermSort.value)([pureMetaExpr(new CtxConsSort(dataa))([v2[0], v2[2]]), v2[0]]), pureMetaExpr(TermSort.value)([pureMetaExpr(new CtxConsSort(dataa))([v2[0], v2[2]]), v2[1]])], pureMetaExpr(TermSort.value)([v2[2], v2[1]]));
              }
              ;
              throw new Error("Failed pattern match at Language (line 183, column 56 - line 188, column 30): " + [v2.constructor.name]);
            };
          }));
        }
        ;
        if (label === "LET_NOANN") {
          return pure12(makeRule(["a", "b", "gamma"])(function() {
            return function(v2) {
              if (v2.length === 3) {
                return new Tuple([pureMetaExpr(TermSort.value)([pureMetaExpr(new CtxConsSort(dataa))([v2[0], v2[2]]), v2[0]]), pureMetaExpr(TermSort.value)([pureMetaExpr(new CtxConsSort(dataa))([v2[0], v2[2]]), v2[1]])], pureMetaExpr(TermSort.value)([v2[2], v2[1]]));
              }
              ;
              throw new Error("Failed pattern match at Language (line 190, column 62 - line 194, column 30): " + [v2.constructor.name]);
            };
          }));
        }
        ;
        if (label === "NOT") {
          return pure12(makeRule(["gamma"])(function() {
            return function(v2) {
              if (v2.length === 1) {
                return new Tuple([pureMetaExpr(TermSort.value)([v2[0], pureMetaExpr(new DataType(Bool.value))([])])], pureMetaExpr(TermSort.value)([v2[0], pureMetaExpr(new DataType(Bool.value))([])]));
              }
              ;
              throw new Error("Failed pattern match at Language (line 196, column 46 - line 199, column 48): " + [v2.constructor.name]);
            };
          }));
        }
        ;
        if (label === "BOOL") {
          return pure12(makeRule([])(function() {
            return function(v2) {
              if (v2.length === 0) {
                return new Tuple([], pureMetaExpr(TypeSort.value)([pureMetaExpr(new DataType(Bool.value))([])]));
              }
              ;
              throw new Error("Failed pattern match at Language (line 211, column 40 - line 214, column 42): " + [v2.constructor.name]);
            };
          }));
        }
        ;
        if (label === "INT") {
          return pure12(makeRule([])(function() {
            return function(v2) {
              if (v2.length === 0) {
                return new Tuple([], pureMetaExpr(TypeSort.value)([pureMetaExpr(new DataType(Int.value))([])]));
              }
              ;
              throw new Error("Failed pattern match at Language (line 216, column 39 - line 219, column 41): " + [v2.constructor.name]);
            };
          }));
        }
        ;
        if (label === "LIST") {
          return pure12(makeRule(["type"])(function() {
            return function(v2) {
              if (v2.length === 1) {
                return new Tuple([pureMetaExpr(TypeSort.value)([v2[0]])], pureMetaExpr(TypeSort.value)([pureMetaExpr(List.value)([v2[0]])]));
              }
              ;
              throw new Error("Failed pattern match at Language (line 221, column 46 - line 224, column 35): " + [v2.constructor.name]);
            };
          }));
        }
        ;
        if (label === "ARROW_TYPE") {
          return pure12(makeRule(["a", "b"])(function() {
            return function(v2) {
              if (v2.length === 2) {
                return new Tuple([pureMetaExpr(TypeSort.value)([v2[0]]), pureMetaExpr(TypeSort.value)([v2[1]])], pureMetaExpr(TypeSort.value)([pureMetaExpr(Arrow.value)([v2[0], v2[1]])]));
              }
              ;
              throw new Error("Failed pattern match at Language (line 226, column 54 - line 229, column 38): " + [v2.constructor.name]);
            };
          }));
        }
        ;
        if (label === "IF") {
          return pure12(makeRule(["gamma", "type"])(function() {
            return function(v2) {
              if (v2.length === 2) {
                return new Tuple([pureMetaExpr(TermSort.value)([v2[0], pureMetaExpr(new DataType(Bool.value))([])]), pureMetaExpr(TermSort.value)([v2[0], v2[1]]), pureMetaExpr(TermSort.value)([v2[0], v2[1]])], pureMetaExpr(TermSort.value)([v2[0], v2[1]]));
              }
              ;
              throw new Error("Failed pattern match at Language (line 231, column 53 - line 236, column 34): " + [v2.constructor.name]);
            };
          }));
        }
        ;
        var v = function(v1) {
          if (label === "INTEGER") {
            return pure12(makeRule(["gamma"])(function() {
              return function(v22) {
                if (v22.length === 1) {
                  return new Tuple([], pureMetaExpr(TermSort.value)([v22[0], pureMetaExpr(new DataType(Int.value))([])]));
                }
                ;
                throw new Error("Failed pattern match at Language (line 243, column 50 - line 246, column 47): " + [v22.constructor.name]);
              };
            }));
          }
          ;
          if (label === "INFIX_EXPRESSION") {
            var v2 = infixTypes(dataa);
            return pure12(makeRule(["gamma"])(function() {
              return function(v3) {
                if (v3.length === 1) {
                  return new Tuple([pureMetaExpr(TermSort.value)([v3[0], v2.left]), pureMetaExpr(TermSort.value)([v3[0], v2.right])], pureMetaExpr(TermSort.value)([v3[0], v2.output]));
                }
                ;
                throw new Error("Failed pattern match at Language (line 250, column 39 - line 253, column 35): " + [v3.constructor.name]);
              };
            }));
          }
          ;
          if (label === "EQUALS") {
            return pure12(makeRule(["gamma", "type"])(function() {
              return function(v22) {
                if (v22.length === 2) {
                  return new Tuple([pureMetaExpr(TermSort.value)([v22[0], v22[1]]), pureMetaExpr(TermSort.value)([v22[0], v22[1]])], pureMetaExpr(TermSort.value)([v22[0], pureMetaExpr(new DataType(Bool.value))([])]));
                }
                ;
                throw new Error("Failed pattern match at Language (line 255, column 57 - line 258, column 48): " + [v22.constructor.name]);
              };
            }));
          }
          ;
          if (label === "NAME" && dataa === "nil") {
            return pure12(makeRule(["gamma", "type"])(function() {
              return function(v22) {
                if (v22.length === 2) {
                  return new Tuple([], pureMetaExpr(TermSort.value)([v22[0], pureMetaExpr(List.value)([v22[1]])]));
                }
                ;
                throw new Error("Failed pattern match at Language (line 260, column 72 - line 263, column 41): " + [v22.constructor.name]);
              };
            }));
          }
          ;
          if (label === "NAME" && dataa === "cons") {
            return pure12(makeRule(["gamma", "type"])(function() {
              return function(v22) {
                if (v22.length === 2) {
                  return new Tuple([], pureMetaExpr(TermSort.value)([v22[0], pureMetaExpr(Arrow.value)([v22[1], pureMetaExpr(Arrow.value)([pureMetaExpr(List.value)([v22[1]]), pureMetaExpr(List.value)([v22[1]])])])]));
                }
                ;
                throw new Error("Failed pattern match at Language (line 265, column 73 - line 268, column 81): " + [v22.constructor.name]);
              };
            }));
          }
          ;
          if (label === "NAME" && dataa === "head") {
            return pure12(makeRule(["gamma", "type"])(function() {
              return function(v22) {
                if (v22.length === 2) {
                  return new Tuple([], pureMetaExpr(TermSort.value)([v22[0], pureMetaExpr(Arrow.value)([pureMetaExpr(List.value)([v22[1]]), v22[1]])]));
                }
                ;
                throw new Error("Failed pattern match at Language (line 270, column 73 - line 273, column 56): " + [v22.constructor.name]);
              };
            }));
          }
          ;
          if (label === "NAME" && dataa === "tail") {
            return pure12(makeRule(["gamma", "type"])(function() {
              return function(v22) {
                if (v22.length === 2) {
                  return new Tuple([], pureMetaExpr(TermSort.value)([v22[0], pureMetaExpr(Arrow.value)([pureMetaExpr(List.value)([v22[1]]), pureMetaExpr(List.value)([v22[1]])])]));
                }
                ;
                throw new Error("Failed pattern match at Language (line 275, column 73 - line 278, column 66): " + [v22.constructor.name]);
              };
            }));
          }
          ;
          if (label === "NAME" && dataa === "index") {
            return pure12(makeRule(["gamma", "type"])(function() {
              return function(v22) {
                if (v22.length === 2) {
                  return new Tuple([], pureMetaExpr(TermSort.value)([v22[0], pureMetaExpr(Arrow.value)([pureMetaExpr(List.value)([v22[1]]), pureMetaExpr(Arrow.value)([pureMetaExpr(new DataType(Int.value))([]), v22[1]])])]));
                }
                ;
                throw new Error("Failed pattern match at Language (line 280, column 74 - line 283, column 87): " + [v22.constructor.name]);
              };
            }));
          }
          ;
          if (label === "NAME" && dataa === "length") {
            return pure12(makeRule(["gamma", "type"])(function() {
              return function(v22) {
                if (v22.length === 2) {
                  return new Tuple([], pureMetaExpr(TermSort.value)([v22[0], pureMetaExpr(Arrow.value)([pureMetaExpr(List.value)([v22[1]]), pureMetaExpr(new DataType(Int.value))([])])]));
                }
                ;
                throw new Error("Failed pattern match at Language (line 285, column 75 - line 288, column 72): " + [v22.constructor.name]);
              };
            }));
          }
          ;
          if (label === "NAME" && dataa === "append") {
            return pure12(makeRule(["gamma", "type"])(function() {
              return function(v22) {
                if (v22.length === 2) {
                  return new Tuple([], pureMetaExpr(TermSort.value)([v22[0], pureMetaExpr(Arrow.value)([pureMetaExpr(List.value)([v22[1]]), pureMetaExpr(Arrow.value)([pureMetaExpr(List.value)([v22[1]]), pureMetaExpr(List.value)([v22[1]])])])]));
                }
                ;
                throw new Error("Failed pattern match at Language (line 290, column 75 - line 293, column 91): " + [v22.constructor.name]);
              };
            }));
          }
          ;
          if (label === "NAME") {
            var v2 = findInCtx(partialSort)(dataa);
            if (v2 instanceof Nothing) {
              return new Left(new UnboundVariable(dataa));
            }
            ;
            if (v2 instanceof Just) {
              return pure12(makeRule(["gamma"])(function() {
                return function(v3) {
                  if (v3.length === 1) {
                    return new Tuple([], pureMetaExpr(TermSort.value)([v3[0], v2.value0]));
                  }
                  ;
                  throw new Error("Failed pattern match at Language (line 301, column 21 - line 303, column 59): " + [v3.constructor.name]);
                };
              }));
            }
            ;
            throw new Error("Failed pattern match at Language (line 296, column 13 - line 303, column 59): " + [v2.constructor.name]);
          }
          ;
          if (label === "MATCH") {
            return pure12(makeRule(["gamma", "type", "outTy"])(function() {
              return function(v22) {
                if (v22.length === 3) {
                  return new Tuple([pureMetaExpr(TermSort.value)([v22[0], pureMetaExpr(List.value)([v22[1]])]), pureMetaExpr(TermSort.value)([v22[0], v22[2]]), pureMetaExpr(TermSort.value)([pureMetaExpr(new CtxConsSort(dataa))([v22[1], pureMetaExpr(new CtxConsSort(dataa2))([pureMetaExpr(List.value)([v22[1]]), v22[0]])]), v22[2]])], pureMetaExpr(TermSort.value)([v22[0], v22[2]]));
                }
                ;
                throw new Error("Failed pattern match at Language (line 305, column 65 - line 310, column 34): " + [v22.constructor.name]);
              };
            }));
          }
          ;
          return bug("language constructor not found: " + label);
        };
        if (label === "NAME") {
          var $311 = constantType(dataa);
          if ($311 instanceof Just) {
            return pure12(makeRule(["gamma"])(function() {
              return function(v1) {
                if (v1.length === 1) {
                  return new Tuple([], pureMetaExpr(TermSort.value)([v1[0], $311.value0]));
                }
                ;
                throw new Error("Failed pattern match at Language (line 238, column 79 - line 241, column 30): " + [v1.constructor.name]);
              };
            }));
          }
          ;
          return v(true);
        }
        ;
        return v(true);
      };
    };
  };
};

// output/Control.Monad.Error.Class/index.js
var throwError = function(dict) {
  return dict.throwError;
};
var monadThrowEither = /* @__PURE__ */ function() {
  return {
    throwError: Left.create,
    Monad0: function() {
      return monadEither;
    }
  };
}();

// output/Control.Monad.State.Class/index.js
var state = function(dict) {
  return dict.state;
};
var modify3 = function(dictMonadState) {
  var state1 = state(dictMonadState);
  return function(f) {
    return state1(function(s) {
      var s$prime = f(s);
      return new Tuple(s$prime, s$prime);
    });
  };
};
var get = function(dictMonadState) {
  return state(dictMonadState)(function(s) {
    return new Tuple(s, s);
  });
};

// output/Control.Monad.State.Trans/index.js
var functorStateT = function(dictFunctor) {
  var map10 = map(dictFunctor);
  return {
    map: function(f) {
      return function(v) {
        return function(s) {
          return map10(function(v1) {
            return new Tuple(f(v1.value0), v1.value1);
          })(v(s));
        };
      };
    }
  };
};
var monadStateT = function(dictMonad) {
  return {
    Applicative0: function() {
      return applicativeStateT(dictMonad);
    },
    Bind1: function() {
      return bindStateT(dictMonad);
    }
  };
};
var bindStateT = function(dictMonad) {
  var bind4 = bind(dictMonad.Bind1());
  return {
    bind: function(v) {
      return function(f) {
        return function(s) {
          return bind4(v(s))(function(v1) {
            var v3 = f(v1.value0);
            return v3(v1.value1);
          });
        };
      };
    },
    Apply0: function() {
      return applyStateT(dictMonad);
    }
  };
};
var applyStateT = function(dictMonad) {
  var functorStateT1 = functorStateT(dictMonad.Bind1().Apply0().Functor0());
  return {
    apply: ap(monadStateT(dictMonad)),
    Functor0: function() {
      return functorStateT1;
    }
  };
};
var applicativeStateT = function(dictMonad) {
  var pure5 = pure(dictMonad.Applicative0());
  return {
    pure: function(a) {
      return function(s) {
        return pure5(new Tuple(a, s));
      };
    },
    Apply0: function() {
      return applyStateT(dictMonad);
    }
  };
};
var monadStateStateT = function(dictMonad) {
  var pure5 = pure(dictMonad.Applicative0());
  var monadStateT1 = monadStateT(dictMonad);
  return {
    state: function(f) {
      return function($200) {
        return pure5(f($200));
      };
    },
    Monad0: function() {
      return monadStateT1;
    }
  };
};

// output/Control.Monad.State/index.js
var unwrap2 = /* @__PURE__ */ unwrap();
var runState = function(v) {
  return function($18) {
    return unwrap2(v($18));
  };
};

// output/Unification/index.js
var eq2 = /* @__PURE__ */ eq(eqMetaVar);
var lookup4 = /* @__PURE__ */ lookup(ordMetaVar);
var pure4 = /* @__PURE__ */ pure(applicativeEither);
var insert3 = /* @__PURE__ */ insert(ordMetaVar);
var bind3 = /* @__PURE__ */ bind(bindEither);
var sequence2 = /* @__PURE__ */ sequence(traversableArray);
var sequence12 = /* @__PURE__ */ sequence2(applicativeEither);
var throwError2 = /* @__PURE__ */ throwError(monadThrowEither);
var monadStateStateT2 = /* @__PURE__ */ monadStateStateT(monadIdentity);
var modify4 = /* @__PURE__ */ modify3(monadStateStateT2);
var bind22 = /* @__PURE__ */ bind(/* @__PURE__ */ bindStateT(monadIdentity));
var get1 = /* @__PURE__ */ get(monadStateStateT2);
var applicativeStateT2 = /* @__PURE__ */ applicativeStateT(monadIdentity);
var pure23 = /* @__PURE__ */ pure(applicativeStateT2);
var sequence3 = /* @__PURE__ */ sequence2(applicativeStateT2);
var map8 = /* @__PURE__ */ map(functorArray);
var sequence4 = /* @__PURE__ */ sequence(traversableList)(applicativeStateT2);
var map1 = /* @__PURE__ */ map(functorList);
var toUnfoldable2 = /* @__PURE__ */ toUnfoldable(unfoldableList);
var recursiveOccurs = function(dictIsExprLabel) {
  return function(sub2) {
    return function(x) {
      return function(e) {
        var v = function(v1) {
          if (e.value0 instanceof MV && e.value1.length === 0) {
            return eq2(e.value0.value0)(x);
          }
          ;
          return any2(recursiveOccurs(dictIsExprLabel)(sub2)(x))(e.value1);
        };
        if (e.value0 instanceof MV && e.value1.length === 0) {
          var $80 = lookup4(e.value0.value0)(sub2);
          if ($80 instanceof Just) {
            return recursiveOccurs(dictIsExprLabel)(sub2)(x)($80.value0);
          }
          ;
          return v(true);
        }
        ;
        return v(true);
      };
    };
  };
};
var unify = function(dictIsExprLabel) {
  var recursiveOccurs1 = recursiveOccurs(dictIsExprLabel);
  var eq12 = eq(dictIsExprLabel.Ord1().Eq0());
  return function(sub2) {
    return function(v) {
      return function(v1) {
        var v2 = new Tuple(v.value0, v1.value0);
        var v3 = function(v4) {
          var v5 = function(v6) {
            if (v2.value0 instanceof MV && (v2.value1 instanceof MV && eq2(v2.value0.value0)(v2.value1.value0))) {
              return pure4(v);
            }
            ;
            if (v2.value0 instanceof MV && !recursiveOccurs1(sub2.get(unit))(v2.value0.value0)(v1)) {
              var v7 = sub2.set(insert3(v2.value0.value0)(v1)(sub2.get(unit)));
              return pure4(v1);
            }
            ;
            if (v2.value1 instanceof MV) {
              return unify(dictIsExprLabel)(sub2)(v1)(v);
            }
            ;
            if (v2.value0 instanceof MInj && (v2.value1 instanceof MInj && eq12(v2.value0.value0)(v2.value1.value0))) {
              return bind3(sequence12(zipWith2(unify(dictIsExprLabel)(sub2))(v.value1)(v1.value1)))(function(kids$prime) {
                return pure4(new Expr(new MInj(v2.value0.value0), kids$prime));
              });
            }
            ;
            return throwError2(unit);
          };
          if (v2.value1 instanceof MV) {
            var $106 = lookup4(v2.value1.value0)(sub2.get(unit));
            if ($106 instanceof Just) {
              return unify(dictIsExprLabel)(sub2)(v)($106.value0);
            }
            ;
            return v5(true);
          }
          ;
          return v5(true);
        };
        if (v2.value0 instanceof MV) {
          var $112 = lookup4(v2.value0.value0)(sub2.get(unit));
          if ($112 instanceof Just) {
            return unify(dictIsExprLabel)(sub2)($112.value0)(v1);
          }
          ;
          return v3(true);
        }
        ;
        return v3(true);
      };
    };
  };
};
var flattenHelperInsertVar = function(dictIsExprLabel) {
  return function(original) {
    return function(x) {
      return bind22(get1)(function(sub2) {
        var v = lookup4(x)(sub2);
        if (v instanceof Just) {
          return pure23(v.value0);
        }
        ;
        if (v instanceof Nothing) {
          return bind22(function() {
            var v1 = lookup4(x)(original);
            if (v1 instanceof Nothing) {
              return pure23(new Expr(new MV(x), []));
            }
            ;
            if (v1 instanceof Just) {
              return bind22(flattenHelper(dictIsExprLabel)(original)(v1.value0))(function(value) {
                return bind22(modify4(insert3(x)(value)))(function() {
                  return pure23(value);
                });
              });
            }
            ;
            throw new Error("Failed pattern match at Unification (line 35, column 22 - line 40, column 35): " + [v1.constructor.name]);
          }())(function(value) {
            return pure23(value);
          });
        }
        ;
        throw new Error("Failed pattern match at Unification (line 32, column 5 - line 41, column 23): " + [v.constructor.name]);
      });
    };
  };
};
var flattenHelper = function(dictIsExprLabel) {
  return function(v) {
    return function(v1) {
      if (v1.value0 instanceof MV) {
        return flattenHelperInsertVar(dictIsExprLabel)(v)(v1.value0.value0);
      }
      ;
      return bind22(sequence3(map8(flattenHelper(dictIsExprLabel)(v))(v1.value1)))(function(kids$prime) {
        return pure23(new Expr(v1.value0, kids$prime));
      });
    };
  };
};
var flattenSubImpl = function(dictIsExprLabel) {
  var flattenHelperInsertVar1 = flattenHelperInsertVar(dictIsExprLabel);
  return function(original) {
    return bind22(sequence4(map1(function(v) {
      return flattenHelperInsertVar1(original)(v.value0);
    })(toUnfoldable2(original))))(function() {
      return pure23(unit);
    });
  };
};
var flattenSub = function(dictIsExprLabel) {
  var flattenSubImpl1 = flattenSubImpl(dictIsExprLabel);
  return function(sub2) {
    return snd(runState(flattenSubImpl1(sub2))(empty2));
  };
};

// output/Typecheck/index.js
var map9 = /* @__PURE__ */ map(functorArray);
var subMetaExprPartially2 = /* @__PURE__ */ subMetaExprPartially(isExprLabelSortLabel);
var flattenSub2 = /* @__PURE__ */ flattenSub(isExprLabelSortLabel);
var unify2 = /* @__PURE__ */ unify(isExprLabelSortLabel);
var trace2 = /* @__PURE__ */ trace();
var fromFoldable3 = /* @__PURE__ */ fromFoldable(foldableList);
var test = function(v) {
  return "(" + (v.label + (" [" + (v.dataa + ("]" + ("[" + (v.dataa2 + ("]" + (joinWith(" ")(map9(function($79) {
    return test(ast2IsAst($79));
  })(v.kids)) + ")"))))))));
};
var printType = function(v) {
  if (v.value0 instanceof MInj && (v.value0.value0 instanceof TermSort && v.value1.length === 2)) {
    return printSort(v["value1"][1]);
  }
  ;
  return bug("printType: wasn't of right form: " + printSort(v));
};
var inferImpl = function(errors) {
  return function(sub2) {
    return function(partialSort) {
      return function(v) {
        var addErrorMsg = function(msg) {
          return errors.set(new Cons({
            start_pos: v.start_pos,
            end_pos: v.end_pos,
            msg
          }, errors.get(unit)));
        };
        var v1 = language(subMetaExprPartially2(flattenSub2(sub2.get(unit)))(partialSort))(v.label)(v.dataa)(v.dataa2);
        if (v1 instanceof Left) {
          return addErrorMsg(v1.value0);
        }
        ;
        if (v1 instanceof Right) {
          var v2 = function() {
            var v32 = unify2(sub2)(v1.value0.value1)(partialSort);
            if (v32 instanceof Left) {
              return addErrorMsg(new UnificationError(v1.value0.value1, partialSort));
            }
            ;
            if (v32 instanceof Right) {
              var $59 = v.label === "HOLE";
              if ($59) {
                return addErrorMsg(new HoleMessage(v1.value0.value1));
              }
              ;
              return unit;
            }
            ;
            throw new Error("Failed pattern match at Typecheck (line 71, column 21 - line 76, column 34): " + [v32.constructor.name]);
          }();
          var v3 = zipWith2(inferImpl(errors)(sub2))(v1.value0.value0)(map9(ast2IsAst)(v.kids));
          return unit;
        }
        ;
        throw new Error("Failed pattern match at Typecheck (line 68, column 5 - line 79, column 17): " + [v1.constructor.name]);
      };
    };
  };
};
var typecheckTop = function(ast) {
  return trace2("Typechecking ast: " + test(ast))(function(v) {
    var errors = stateful(Nil.value);
    var sub2 = stateful(empty2);
    var topSort = new Expr(new MInj(TermSort.value), [new Expr(new MInj(CtxNilSort.value), []), freshMetaVarSort("topty")]);
    var v1 = inferImpl(errors)(sub2)(topSort)(ast);
    var printError = function(v2) {
      var applySubs = subMetaExprPartially2(flattenSub2(sub2.get(unit)));
      if (v2.msg instanceof UnificationError) {
        return {
          severity: "error",
          msg: "Type error. Actual type: " + (printType(applySubs(v2.msg.value0)) + ("   Expected type: " + printType(applySubs(v2.msg.value1)))),
          start_pos: {
            line: v2.start_pos.line,
            overallPos: v2.start_pos.overallPos,
            offset: v2.start_pos.offset + 1
          },
          end_pos: {
            line: v2.end_pos.line,
            overallPos: v2.end_pos.overallPos,
            offset: v2.end_pos.offset + 1
          }
        };
      }
      ;
      if (v2.msg instanceof UnboundVariable) {
        return {
          severity: "error",
          msg: "Unbound variable: " + v2.msg.value0,
          start_pos: {
            line: v2.start_pos.line,
            overallPos: v2.start_pos.overallPos,
            offset: v2.start_pos.offset + 1
          },
          end_pos: {
            line: v2.end_pos.line,
            overallPos: v2.end_pos.overallPos,
            offset: v2.end_pos.offset + 1
          }
        };
      }
      ;
      if (v2.msg instanceof HoleMessage) {
        return {
          severity: "warning",
          msg: "Hole with type: " + printType(applySubs(v2.msg.value0)),
          start_pos: {
            line: v2.start_pos.line,
            overallPos: v2.start_pos.overallPos,
            offset: v2.start_pos.offset + 1
          },
          end_pos: {
            line: v2.end_pos.line,
            overallPos: v2.end_pos.overallPos,
            offset: v2.end_pos.offset + 1
          }
        };
      }
      ;
      throw new Error("Failed pattern match at Typecheck (line 96, column 13 - line 114, column 21): " + [v2.msg.constructor.name]);
    };
    return map9(printError)(fromFoldable3(errors.get(unit)));
  });
};
export {
  evaluate,
  typecheckTop
};
