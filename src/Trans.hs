module Trans where

import Term
import Exception
import Data.List
import Data.Maybe
import Debug.Trace

transform n (t,d) = returnval (trans n t EmptyCtx (free t) [] d)

trans 0 t k fv m d = return (place t k)
trans n t (ApplyCtx k []) fv m d = trans n t k fv m d
trans n (Free x) (CaseCtx k bs) fv m d = do
                                         bs' <- mapM (\(c,xs,t) -> let t' = place t k
                                                                       fv' = foldr (\x fv -> let x' = rename fv x in x':fv) fv xs
                                                                       xs' = take (length xs) fv'
                                                                       u = subst (Con c (map Free xs')) (abstract (foldr concrete t' xs') x)
                                                                   in do
                                                                      u' <- trans n u EmptyCtx fv' m d
                                                                      return (c,xs,foldl abstract u' xs')) bs
                                         return (Case (Free x) bs')
trans n (Free x) k fv m d = transCtx n (Free x) k fv m d
trans n (Lambda x t) EmptyCtx fv m d = let x' = rename fv x
                                       in do
                                          t' <- trans n (concrete x' t) EmptyCtx (x':fv) m d
                                          return (Lambda x (abstract t' x'))
trans n (Lambda x t) (ApplyCtx k (t':ts)) fv m d = trans n (subst t' t) (ApplyCtx k ts) fv m d
trans n (Lambda x t) (CaseCtx k bs) fv m d = error "Unapplied function in case selector"
trans n (Con c ts) EmptyCtx fv m d = do
                                     ts' <- mapM (\t -> trans n t EmptyCtx fv m d) ts
                                     return (Con c ts')
trans n (Con c ts) (ApplyCtx k ts') fv m d = error ("Constructor application is not saturated: "++show (place (Con c ts) (ApplyCtx k ts')))
trans n (Con c ts) (CaseCtx k bs) fv m d = case find (\(c',xs,t) -> c==c' && length xs == length ts) bs of
                                              Nothing -> error ("No matching pattern in case for term:\n\n"++show (Case (Con c ts) bs))
                                              Just (c',xs,t) -> trans n (foldr subst t ts) k fv m d
trans n (Fun f) k fv m d | f `notElem` fst(unzip d) = transCtx n (Fun f) k fv m d
trans n (Fun f) k fv m d = let t = returnval (trans (n-1) (Fun f) k fv [] d)
                           in case find (\(f,(xs,t')) -> isJust (inst t' t)) m of
                                 Just (f,(xs,t')) -> let Just s = inst t' t
                                                     in  return (instantiate s (Apply (Fun f) (map Free xs)))
                                 Nothing -> case find (\(f,(xs,t')) -> not (null (embed t' t))) m of
                                               Just (f,_) -> throw (f,t)
                                               Nothing -> let fs = fst(unzip (m++d))
                                                              f = rename fs "f"
                                                              xs = free t
                                                              (t',d') = unfold t (f:fs) d
                                                              handler (f',t') = if   f==f'
                                                                                then let (t'',s1,s2) = generalise t t'
                                                                                     in  trans n (makeLet s1 t'') EmptyCtx fv m d
                                                                                else throw (f',t')
                                                          in  do t'' <- handle (trans n t' EmptyCtx fv ((f,(xs,t)):m) d') handler
                                                                 return (if f `elem` funs t'' then Letrec f xs (foldl abstract (abstractFun t'' f) xs) (Apply (Bound 0) (map Free xs)) else t'')
trans n (Apply t ts) k fv m d = trans n t (ApplyCtx k ts) fv m d
trans n (Case t bs) k fv m d = trans n t (CaseCtx k bs) fv m d
trans n (Let x t u) k fv m d = let x' = rename fv x
                                in do
                                   t' <- trans n t EmptyCtx fv m d
                                   u' <- trans n (concrete x' u) k (x':fv) m d
                                   return (subst t' (abstract u' x'))
trans n (Letrec f xs t u) k fv m d = let f' = rename (fst(unzip(m++d))) f
                                         t' = concreteFun f' (foldr concrete t xs)
                                         u' = concreteFun f' u
                                     in  trans n u' k (f':fv) m ((f',(xs,t')):d)

transCtx n t EmptyCtx fv m d = return t
transCtx n t (ApplyCtx k ts) fv m d = do
                                      ts' <- mapM (\t -> trans n t EmptyCtx fv m d) ts
                                      transCtx n (Apply t ts') k fv m d
transCtx n t (CaseCtx k bs) fv m d = do
                                     bs' <- mapM (\(c,xs,t) -> let fv' = foldr (\x fv -> let x' = rename fv x in x':fv) fv xs
                                                                   xs' = take (length xs) fv'
                                                               in do
                                                                  t' <- trans n (foldr concrete t xs') k fv' m d
                                                                  return (c,xs,foldl abstract t' xs')) bs
                                     return (Case t bs')
