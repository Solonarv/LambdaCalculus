{-# LANGUAGE
    TemplateHaskell
    #-}

module Data.Function.Lambda.Untyped.Predefs where
-- Church numerals, pairs, ...
-- Note: omitted type signatures are :: U

import Data.Function.Lambda.Untyped.Core
import Data.Function.Lambda.Untyped.TH

$(mkLiftLs 10)

constU = liftL2 const
idU = liftL1 id
compose = liftL3 $ \f g x -> f $$! (g $$! x)


-- #####################
-- #  Church booleans  #
-- #####################

pair = liftL3 $ \x y f -> f $$! x $$! y
true = constU                                   -- Will also retrieve the first item from a pair
false = czero                                   -- Will also retrieve the second item from a pair
notU = liftL3 $ \f x y -> f $$! y $$! x              -- Also serves as the equivalent of flip
andU = liftL2 $ \x y -> x $$! y $$! x
orU = liftL2 $ \x y -> x $$! x $$! y
xorU = liftL2 $ \x y -> andU $$! (orU $$! x $$! y) $$! (notU $$! (andU $$! x $$! y))

-- #####################
-- #  Church numerals  #
-- #####################

-- Natural numbers
czero = constU $$! idU; cone = csucc $$! czero
csucc = liftL3 $ \n f x -> f $$! (n $$! f $$! x)
cplus = liftL1 $ \n -> n $$! csucc
cmult = liftL2 $ \n m -> m $$! (cplus $$! n) $$! czero
cpred = liftL1 $ \n -> true $$! (n $$! (lift $ \p -> pair $$! (p $$! false)
                                                      $$! (csucc $$! (p $$! false)))
                                $$! (pair $$! czero $$! czero))
cminus = liftL1 $ \n -> n $$! cpred

ciszero = liftL1 $ \n -> n $$! (constU $$! false) $$! true
cleq = liftL2 $ \m n -> ciszero $$! (cminus $$! m $$! n)
ceq = liftL2 $ \m n -> andU $$! (cleq $$! m $$! n) $$! (cleq $$! n $$! m)

-- Integers
ntoz = liftL1 $ \n -> pair $$! n $$! czero
zzero = pair $$! czero $$! czero
zplus = liftL2 $ \m n -> pair $$! (cplus $$! (m $$! true)
                                      $$! (n $$! true))
                            $$! (cplus $$! (m $$! false)
                                      $$! (n $$! false))
zeq = liftL2 $ \m n -> ceq $$! (cplus $$! (m $$! true)
                                   $$! (n $$! false))
                         $$! (cplus $$! (m $$! false)
                                   $$! (n $$! true))
zneg = liftL1 $ \n -> compose $$! n $$! notU
zmul = liftL2 $ \m n -> pair $$! (cplus $$! (cmult $$! (m $$! true)  $$! (n $$! true))
                                    $$! (cmult $$! (m $$! false) $$! (n $$! false)))
                              $$! (cplus $$! (cmult $$! (m $$! true)  $$! (n $$! false))
                                    $$! (cmult $$! (m $$! false) $$! (n $$! true)))
zleq = liftL2 $ \m n -> cleq $$! (cplus $$! (m $$! true)
                                     $$! (n $$! false))
                           $$! (cplus $$! (m $$! false)
                                     $$! (n $$! true))
ziszero = liftL1 $ \n -> ceq $$! (n $$! true) $$! (n $$! false)
zisnonnegative = liftL1 $ \n -> cleq $$! (n $$! false) $$! (n $$! true)

instance Num U where
    x + y = zplus $$! x $$! y
    x * y = zmul $$! x $$! y
    abs x = zisnonnegative $$! x $$! x $$! (zneg $$! x)
    signum x = zisnonnegative $$! x $$! (ntoz $$! cone) $$! (pair $$! czero $$! cone)
    fromInteger n = let cn = (iterate (csucc $$!) czero) !! fromInteger n
                    in if n < 0 then pair $$! czero $$! cn else ntoz $$! cn
    negate = (zneg $$!)

-- Rationals sometime else

-- Some combinators

-- Anonymous recursion made easy!
anonrec = liftL2 $ \f r -> f $$ f $$ r
-- Ye Ole Y Combinator
fixedpoint = liftL1 $ \f -> innercomb $$ innercomb where innercomb = liftL1 $ \x -> f $$ (x $$ x)
-- SKI, aliased for convenience
s = liftL3 $ \x y z -> (x $$ z) $$ (y $$ z)
k = constU
i = idU