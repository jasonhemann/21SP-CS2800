module FactFib
import Decidable.Order
%default total

LTEMonoInRight : {n,m : Nat} -> LTE n m -> LTE n (S m)
LTEMonoInRight {n = Z} _ = LTEZero
LTEMonoInRight {n = S k} {m = Z} x = LTEIsTransitive (S k) Z (S Z) x LTEZero
LTEMonoInRight (LTESucc x) = LTESucc (LTEMonoInRight x)

LTEMonoInRightPlus : {n,m,o : Nat} -> LTE n m -> LTE n (o + m)
LTEMonoInRightPlus {o = Z} x = x
LTEMonoInRightPlus {o = S k} x = LTEMonoInRight (LTEMonoInRightPlus x)

LTEMonoInRightSMult : {n,m,o : Nat} -> LTE n m -> LTE n (S o * m)
LTEMonoInRightSMult {m} {o = Z} x = rewrite multOneLeftNeutral m in x
LTEMonoInRightSMult {m} {o = S k} x =
  rewrite plusCommutative m (m + (k * m)) in
    LTEMonoInRightPlus x

LTEPlus : {n,m,o,p : Nat} -> LTE n m -> LTE o p -> LTE (n + o) (m + p)
LTEPlus LTEZero LTEZero = LTEZero
LTEPlus {n = S left} {m = S right} {p = Z} (LTESucc x) LTEZero =
  LTESucc
    (rewrite plusZeroRightNeutral left in
       rewrite plusZeroRightNeutral right in
          x)
LTEPlus {n = S left} {m = S right} {p = S k} (LTESucc x) LTEZero =
  LTESucc
    (rewrite plusZeroRightNeutral left in
       rewrite plusCommutative right (S k) in
         LTEMonoInRightPlus {o = S k} x)
LTEPlus {m = Z} {o = S left} {p = S right} LTEZero (LTESucc y) =
  LTESucc (LTEPlus {m = Z} {o = left} {p = right} LTEZero y)
LTEPlus {m = S k} {o = S left} {p = S right} LTEZero (LTESucc y) =
  LTESucc
    (rewrite sym (plusSuccRightSucc k right) in
       LTEMonoInRightPlus {o = S k} y)
LTEPlus {n = S k} {m = S j} {o = S left} {p = S right} (LTESucc x) (LTESucc y) =
  LTESucc
    (rewrite plusCommutative k (S left) in
       rewrite plusCommutative j (S right) in
          LTESucc
            (rewrite plusCommutative left k in
               rewrite plusCommutative right j in
                 LTEPlus x y))

fibLTEFact : (n : Nat) -> LTE (fib n) (fact n)
fibLTEFact Z = LTEZero
fibLTEFact (S Z) = LTEIsReflexive (S Z)
fibLTEFact (S (S k)) =
  rewrite plusCommutative (fib (S k)) (fib k) in
    rewrite plusCommutative (fact k) (mult k (fact k)) in
      LTEPlus
        {p = S k * ((k * (fact k)) + (fact k))}
        (LTEMonoInRightPlus (fibLTEFact k))
        (rewrite plusCommutative (k * (fact k)) (fact k) in
           (LTEIsTransitive
              (fib (S k))
              (fact (S k))
              (S k * (fact (S k)))
              (fibLTEFact (S k))
              (LTEMonoInRightSMult (LTEIsReflexive (fact (S k))))))
              
