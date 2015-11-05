module LambdaCalculus.Untyped.Predefined where
-- Church numerals, pairs, ...
-- Note: omitted type signatures are :: U

import Untyped

constU = lift const
idU = lift id
compose = lift $ \f g x -> f $$ (g $$ x)


-- #####################
-- #  Church booleans  #
-- #####################

pair = lift $ \x y f -> f $$ x $$ y
true = constU                                   -- Will also retrieve the first item from a pair
false = czero                                   -- Will also retrieve the second item from a pair
notU = lift $ \f x y -> f $$ y $$ x              -- Also serves as the equivalent of flip
andU = lift $ \x y -> x $$ y $$ x
orU = lift $ \x y -> x $$ x $$ y
xorU = lift $ \x y -> andU $$ (orU $$ x $$ y) $$ (notU $$ (andU $$ x $$ y))

-- #####################
-- #  Church numerals  #
-- #####################

-- Natural numbers
czero = constU $$ idU; cone = csucc $$ czero
csucc = lift $ \n f x -> f $$ (n $$ f $$ x)
cplus = lift $ \n -> n $$ csucc
cmult = lift $ \n m -> m $$ (cplus $$ n) $$ czero
cpred = lift $ \n -> true $$ (n $$ (lift $ \p -> pair $$ (p $$ false)
                                                      $$ (csucc $$ (p $$ false)))
                                $$ (pair $$ czero $$ czero))
cminus = lift $ \n -> n $$ pred

ciszero = lift $ \n -> n $$ (constU $$ false) $$ true
cleq = lift $ \m n -> ciszero $$ (cminus $$ m $$ n)
ceq = lift $ \m n -> andU $$ (cleq $$ m $$ n) $$ (cleq $$ n $$ m)

-- Integers
ntoz = lift $ \n -> pair $$ n $$ czero
zzero = pair $$ czero $$ czero
zplus = lift $ \m n -> pair $$ (cplus $$ (m $$ true)
                                      $$ (n $$ true))
                            $$ (cplus $$ (m $$ false)
                                      $$ (n $$ false))
zeq = lift $ \m n -> ceq $$ (cplus $$ (m $$ true)
                                   $$ (n $$ false))
                         $$ (cplus $$ (m $$ false)
                                   $$ (n $$ true))
zneg = lift $ \n -> compose $$ n $$ notU
zmul = lift $ \m n -> pair $$ (cplus $$ (cmult $$ (m $$ true)  $$ (n $$ true))
                                    $$ (cmult $$ (m $$ false) $$ (n $$ false)))
                              (cplus $$ (cmult $$ (m $$ true)  $$ (n $$ false))
                                    $$ (cmult $$ (m $$ false) $$ (n $$ true)))
zleq = lift $ \m n -> cleq $$ (cplus $$ (m $$ true)
                                     $$ (n $$ false))
                           $$ (cplus $$ (m $$ false)
                                     $$ (n $$ true))
ziszero = lift $ \n -> ceq $$ (n $$ true) $$ (n $$ false)
zisnonnegative = lift $ \n -> cleq $$ (n $$ false) $$ (n $$ true)

instance Num U where
    x + y = zplus $$ x $$ y
    x * y = zmul $$ x $$ y
    abs x = zisnonnegative $$ x $$ x $$ (zneg $$ x)
    signum x = zisnonnegative $$ x $$ (ntoz $$ cone) $$ (pair $$ czero $$ cone)
    fromInteger n = let cn = (iterate (csucc $$) czero) !! fromInteger n
                    in if n < 0 then pair $$ czero $$ cn else ntoz $$ cn
    negate = (zneg $$)

-- Rationals sometime else