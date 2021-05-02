-- Taylor Approximation

factorial :: Integer -> Integer
factorial n = if n > 0
              then n * factorial (n-1)
              else 1

{- -----------------------------------------------------------------
 - cosTaylor
 - -----------------------------------------------------------------
 - Description: cosTaylor gives an approximation of what the cos function would look at various points of x
   in which approximations are calculated from point 'a'. cos_a and sin_a values are determined based on the value of 'a'.
 -}

cosTaylor :: Double -> Double -> Double -> Double -> Double
cosTaylor a cos_a sin_a x = ((cos_a/(fromIntegral(factorial 0))) * (x-a)^0) 
                            - ((sin_a/(fromIntegral(factorial 1))) * (x-a)^1) 
                            - ((cos_a/(fromIntegral(factorial 2))) * (x-a)^2) 
                            + ((sin_a/(fromIntegral(factorial 3))) * (x-a)^3) 
                            + ((cos_a/(fromIntegral(factorial 4))) * (x-a)^4) 


{- -----------------------------------------------------------------
 - fmod
 - -----------------------------------------------------------------
 - Description: As the function takes a double parameter, use the formuale :
   Remainder = Dividend - (Divisor * Quotient) to compute the fmod value. We use the floor function 
   to find the simplest quotient.
-}

fmod :: Double -> Double -> Double
fmod x y = x - (y * (fromIntegral (floor (x/y)))) 


{- -----------------------------------------------------------------
 - cosApprox
 - -----------------------------------------------------------------
 - Description: The cosApprox function is similar to the cosTaylor function, the only difference being that 
   we centered the approximation of 'a' depending on the fmod interval. 'a', cos_a and sin_a values 
   are taken based on the interval that "fmod (abs(x)) (2*pi)" belongs to (part 5 of assignment).
 -}

-- fmod2 is a function that uses fmod x y, in which x = (abs(x)) and y = (2*pi).
fmod2 :: Double -> Double
fmod2 x = fmod (abs(x)) (2*pi)

cosApprox :: Double -> Double
cosApprox x 
    | fmod2 x >= 0 && fmod2 x < (pi/4) = cosTaylor 0 1 0 (fmod2 x) 
    | fmod2 x >= (pi/4) && fmod2 x < ((3*pi)/4) = cosTaylor (pi/2) 0 1 (fmod2 x) 
    | fmod2 x >= ((3*pi)/4) && fmod2 x < ((5*pi)/4) = cosTaylor (pi) (-1) 0 (fmod2 x) 
    | fmod2 x >= ((5*pi)/4) && fmod2 x < ((7*pi)/4) = cosTaylor ((3*pi)/2) 0 (-1) (fmod2 x) 
    | fmod2 x >= ((7*pi)/4) && fmod2 x < (2*pi) = cosTaylor (2*pi) 1 0 (fmod2 x) 
    | otherwise = undefined
                    

{- -----------------------------------------------------------------
 - sinApprox
 - -----------------------------------------------------------------
 - Description: We use the formula sin(x) = cos((pi/2)-x), to approximate the sine value at x.
   To do this, we use the cosApprox function to find the corresponding sinApprox value.
 -}

sinApprox :: Double -> Double
sinApprox x = cosApprox ((pi/2) - x)


{- -----------------------------------------------------------------
 - tanApprox
 - -----------------------------------------------------------------
- Description: The tanApprox function uses the formula: tan(x) = sin(x) / cos(x), which aids in computing the tanApprox
  value at x.
-}

tanApprox :: Double -> Double
tanApprox x = (sinApprox x)/(cosApprox x)


{- -----------------------------------------------------------------
 - Test Cases

-----------------------------------------------------------------
Test Cases for Cosine against cosApprox

#Test Case-1 for cosApprox
cos(0) = 1.0 and cos(2*pi) = 1.0
cosApprox (0) = 1.0 and cosApprox (2*pi) = 1.0

#Test Case-2 for cosApprox
cos(pi/6) = 0.866025403784438 and cos(-pi/6) = 0.8660254037844387
cosApprox (pi/6) = 0.8660538834157472 and cosApprox (-pi/6) = 0.8660538834157472

#Test Case-3 for cosApprox
cos(pi/2) = 6.123233995736766e-17 and cos(-pi/2) = 6.123233995736766e-17 
cosApprox (pi/2) = 0.0 and cosApprox (-pi/2) = 0.0

#Test Case-4 for cosApprox
cos(pi) = -1.0
cosApprox (pi) = -1.0

#Test Case-5 for cosApprox
cos((5*pi)/6) = -0.8660254037844387
cosApprox ((5*pi)/6) = -0.8660538834157471

-----------------------------------------------------------------
Test Cases for Sine against sinApprox

#Test Case-1 for sinApprox
sin(0) = 0.0 and sin(2*pi) = -2.4492935982947064e-16
sinApprox (0) = 0.0 and sinApprox (2*pi) = 0.0

#Test Case-2 for sinApprox
sin(pi/6) = 0.49999999999999994 and sin(-pi/6) = -0.49999999999999994
sinApprox (pi/6) = 0.4996741793943637 and sinApprox (-pi/6) = -0.4996741793943637

#Test Case-3 for sinApprox
sin(pi/2) = 1.0 and sin(-pi/2) = -1.0
sinApprox (pi/2) = 1.0 and sinApprox (-pi/2) = -1.0

#Test Case-4 for sinApprox
sin(pi) = 1.2246467991473532e-16 
sinApprox (pi) = 0.0

#Test Case-5 for sinApprox
sin((5*pi)/6) = 0.49999999999999994
sinApprox ((5*pi)/6) = 0.4996741793943637

-----------------------------------------------------------------
Test Cases for Tangent against tanApprox

#Test Case-1 for tanApprox
tan(0) = 0.0 and tan(2*pi) = -2.4492935982947064e-16
tanApprox (0) = 0.0 and tanApprox (2*pi) = 0.0

#Test Case-2 for tanApprox
tan(pi/6) = 0.5773502691896256 and tan(-pi/6) = -0.5773502691896256
tanApprox (pi/6) = 0.5769550705362939 and tanApprox (-pi/6) = -0.5769550705362939

#Test Case-3 for tanApprox
tan(pi/2) = 1.633123935319537e16
tanApprox (pi/2) = Infinity

#Test Case-4 for tanApprox
tan(pi) = -1.2246467991473532e-16
tanApprox (pi) = -0.0

#Test Case-5 for tanApprox
tan((5*pi)/6) = -0.5773502691896256
tanApprox ((5*pi)/6) = -0.5769550705362939

- -----------------------------------------------------------------
-}


