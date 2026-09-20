module Main where
    import Data.Complex
    import System.Random
    import Control.Monad (replicateM)
    data Qubit=Qubit (Complex Double) (Complex Double) -- |0> |1>
        deriving (Show,Eq)
    data Qubit2 = Qubit2 (Complex Double) (Complex Double) (Complex Double) (Complex Double) -- |00> |01> |10> |11> 
        deriving (Show, Eq)
    
    i::Complex Double
    i=0 :+ 1
    
    zero::Qubit
    zero=Qubit 1 0

    one :: Qubit
    one =Qubit 0 1

    --1 qubit

    hadamard::Qubit->Qubit
    hadamard (Qubit a b)=Qubit ((a+b)/sqrt 2.0 ) ((a-b)/sqrt 2)
    
    pauliX::Qubit->Qubit
    pauliX (Qubit a b)=Qubit b a

    pauliY::Qubit->Qubit
    pauliY (Qubit a b)=Qubit (negate i *b) (i*a)

    pauliZ::Qubit->Qubit
    pauliZ (Qubit a b)=Qubit a (negate b)

    measure :: Qubit -> IO Bool
    measure (Qubit a _b)=do
        let p0=magnitude a ^2
        r<-randomRIO(0.0,1.0)
        return (r<p0)   

    applyGates :: [Qubit -> Qubit] -> Qubit -> Qubit
    applyGates gates q=go gates q
        where
            go [] q=q
            go (x:xs) q=go xs (x q)

    --double qubit        

    tensor::Qubit->Qubit->Qubit2
    tensor (Qubit a b) (Qubit c e)=Qubit2 (a*c) (a*e) (b*c) (b*e)

    cnot::Qubit2->Qubit2
    cnot (Qubit2 a b c d)=Qubit2 a b d c

    measure2 :: Qubit2 -> IO (Bool, Bool)
    measure2 (Qubit2 a b c _)=do
        r<-randomRIO(0.0,1.0)
        let p00=magnitude a^2  
            p01=magnitude b^2 
            p10=magnitude c^2
        return $ case () of
            _|r<p00->(True,True)
             |r<p00+p01->(True,False)
             |r<p00+p01+p10->(False,True)
             |otherwise->(False,False)
            
                
    main::IO()
    main=do
        print zero
        print one 
        print $ hadamard zero
        print $ hadamard one
        print $ hadamard(hadamard zero)
        print $ pauliX one
        print $ pauliX(pauliX one)
        print $ hadamard (pauliZ (hadamard zero))
        print $ pauliX zero
        r<-measure zero
        if r then print 0 else print 1
        b<-measure one
        if b then print 0 else print 1
        c<-measure(hadamard zero)
        if c then print 0 else print 1
        print $ applyGates [hadamard, pauliZ, hadamard] zero
        print $ pauliX zero
        print $ tensor zero zero 
        print $ tensor zero one
        print $ tensor one zero
        print $ tensor one one 
        print $ cnot (tensor one one)
        let bell=cnot (tensor (hadamard zero) zero)
        print bell
        result<-measure2 (tensor zero zero)
        print result
        result<-measure2 (tensor one one)
        let toInt b=if b then 0 else 1 
        print (toInt (fst result) , toInt (snd result))
        