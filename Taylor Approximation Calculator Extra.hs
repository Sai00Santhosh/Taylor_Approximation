{- Assignment 1 Extra Credit
 - Name: Sai Santhosh Thunga
 - Date: October 4th, 2020
 -}

module Assign_1_ExtraCredit where

macid :: String
macid = "thungas"


factorial :: Integer -> Integer
factorial n = if n > 0
              then n * factorial (n-1)
              else 1

{- -----------------------------------------------------------------
 - cosTaylor
 - -----------------------------------------------------------------
 - Description: The cosTaylor function uses recursion to represent the MacLaurin Series (Special case of Taylor series, in which a = 0) 
   at the nth degree polynomial. This helps in outputing the desired result based on the x and n parameters (n represents the degree of 
   the polynomial). As n increases, the polynomial tries to wrap around cos(x) graph, which consequently reduces the error between 
   the approximation and the actual value.
 -}

cosTaylor :: Fractional p => p -> Integer -> p
cosTaylor _ 0 = 1
cosTaylor x n = ((((-1)^n)/(fromIntegral(factorial (2*n)))) * (x)^(2*n)) + cosTaylor x (n-1)


{- -----------------------------------------------------------------
 - fmod
 - -----------------------------------------------------------------
 - Description: As the function takes a double parameter, we use the formuale :
   Remainder = Dividend - (Divisor * Quotient), to compute the fmod value. we use the floor function 
   to find the simplest quotient.
-}

fmod :: Double -> Double -> Double
fmod x y = x - (y * (fromIntegral (floor (x/y))))


{- -----------------------------------------------------------------
 - tolerance
 - -----------------------------------------------------------------
 - Description: tolerance is a function designed to give a close approximate to cosApprox based on the tolerance 't' inputed.
   If the value isn't within the required tolerance (condition : abs(cosTaylor x n - cos x) < t), the function adds another term (n+1) 
   to cosTaylor to reach the desired tolerance recursively. Note: Increasing n reduces the error term.
 -}

tolerance :: (Ord p, Floating p) => p -> Integer -> p -> p
tolerance x n t 
        | abs(cosTaylor x n - cos x) < t = cosTaylor x n 
        | otherwise = tolerance x (n+1) t


{- -----------------------------------------------------------------
 - cosApprox
 - -----------------------------------------------------------------
 - Description: The cosApprox function is similar to the tolerance function, the only difference being that we substitute (fmod x) 
   in place of x. Also note that n is predefined as 1, hence the tolerance function is recursively calculated until 
   the error is less than 't'.

   The cosApprox function takes two double parameters, x and t, and gives an output of a double that approximates cosApprox value 
   within the tolerance of t.
 -}

-- fmod2 is a function that uses fmod x y, in which x = (abs(x)) and y = (2*pi).
fmod2 :: Double -> Double
fmod2 x = fmod (abs(x)) (2*pi)

cosApprox :: Double -> Double -> Double
cosApprox x t = tolerance (fmod2 x) n t 
          where
              n = 1

{- -----------------------------------------------------------------
 - sinApprox
 - -----------------------------------------------------------------
 - Description: We use the formula sin(x) = cos((pi/2)-x), to approximate the sine value at x.
   To do this, we use the cosApprox function to find the corresponding sinApprox value. As we still use cosApprox to compute
   the sinApprox value, it would still output the sinApprox value within the tolerance of t.
 -}

sinApprox :: Double -> Double -> Double
sinApprox x t = cosApprox (((pi/2) - (x))) t

    
{- -----------------------------------------------------------------
 - tanApprox
 - -----------------------------------------------------------------
- Description: The tanApprox function uses the formula: tan(x) = sin(x) / cos(x), which aid us in computing the tanApprox 
  value at x. Guards are used to give us appropriate tanApprox values at (n * pi/2).

  The tanApprox function takes two double parameters, x and t, and gives an output of a double that approximates tanApprox value 
  within the tolerance of t.
-}

tanApprox :: Double -> Double -> Double
tanApprox x t 
          | fmod2 x == (pi/2) = read "Infinity"
          | fmod2 x == (3*pi/2) = read "Infinity"
          | otherwise = (sinApprox x t)/(cosApprox x t)


{- -----------------------------------------------------------------
 - Test Cases

-----------------------------------------------------------------
Test Cases for Cosine against cosApprox

#Test Case-1 for cosApprox
cos(0) = 1.0 and cos(2*pi) = 1.0
cosApprox (0) 0.001 = 1.0 and cosApprox (2*pi) 0.001 = 1.0
cosApprox (0) 0.000001 = 1.0 and cosApprox (2*pi) 0.000001 = 1.0

#Test Case-2 for cosApprox
cos(pi/6) = 0.866025403784438 and cos(-pi/6) = 0.8660254037844387
cosApprox (pi/6) 0.001 = 0.8660538834157472 and cosApprox (-pi/6) 0.001 = 0.8660538834157472
cosApprox (pi/6) 0.000001 = 0.8660252641005711 and cosApprox (-pi/6) 0.000001 = 0.8660252641005711

#Test Case-3 for cosApprox
cos(pi/2) = 6.123233995736766e-17 and cos(-pi/2) = 6.123233995736766e-17 
cosApprox (pi/2) 0.001 = -8.945229984747317e-4 and cosApprox (-pi/2) 0.001 = -8.945229984747317e-4
cosApprox (pi/2) 0.000001 = -4.647660083659679e-7 and cosApprox (-pi/2) 0.000001 = -4.647660083659679e-7
cosApprox (pi/2) 0.000000000001 = 5.260632071002592e-13 and cosApprox (-pi/2) 0.000000000001 = 5.260632071002592e-13

#Test Case-4 for cosApprox
cos(pi) = -1.0
cosApprox (pi) 0.001 = -0.9998995297042177
cosApprox (pi) 0.000001 = --0.9999998647395555

#Test Case-5 for cosApprox
cos((5*pi)/6) = -0.8660254037844387
cosApprox ((5*pi)/6) 0.001 = -0.86623389610503
cosApprox ((5*pi)/6) 0.000001 = -0.8660256314079756

-----------------------------------------------------------------
Test Cases for Sine against sinApprox

#Test Case-1 for sinApprox
sin(0) = 0.0
sinApprox (0) 0.001 = -8.945229984747317e-4
sinApprox (0) 0.000001 = -4.647660083659679e-7
sinApprox (0) 0.00000000000001 = -3.3768129732028557e-15

#Test Case-2 for sinApprox
sin(pi/6) = 0.49999999999999994 and sin(-pi/6) = -0.49999999999999994
sinApprox (pi/6) 0.001 = 0.4999645653289126 and sinApprox (-pi/6) 0.001 = -0.49956698894078944
sinApprox (pi/6) 0.000001 = 0.5000004334329149 and sinApprox (-pi/6) 0.000001 = -0.4999996480218145

#Test Case-3 for sinApprox
sin(pi/2) = 1.0 and sin(-pi/2) = -1.0
sinApprox (pi/2) 0.001 = 1.0 and sinApprox (-pi/2) 0.001 = -0.9998995297042177
sinApprox (pi/2) 0.000001 = 1.0 and sinApprox (-pi/2) 0.000001 = -0.9999998647395555

#Test Case-4 for sinApprox
sin(pi) = 1.2246467991473532e-16 
sinApprox (pi) 0.001 = -8.945229984747317e-4
sinApprox (pi) 0.000001 = -4.647660083659679e-7

#Test Case-5 for sinApprox
sin((5*pi)/6) = 0.49999999999999994
sinApprox ((5*pi)/6) 0.001 = 0.4999645653289126
sinApprox ((5*pi)/6) 0.000001 = 0.5000004334329149

-----------------------------------------------------------------
Test Cases for Tangent against tanApprox

#Test Case-1 for tanApprox
tan(0) = 0.0
tanApprox (0) 0.001 = -8.945229984747317e-4
tanApprox (0) 0.000001 = -4.647660083659679e-7

#Test Case-2 for tanApprox
tan(pi/6) = 0.5773502691896256 and tan(-pi/6) = -0.5773502691896256
tanApprox (pi/6) 0.001 = 0.5772903683048388 and tanApprox (-pi/6) 0.001 = -0.5768313017320349
tanApprox (pi/6) 0.000001 = 0.57735086279752 and tanApprox (-pi/6) 0.000001 = -0.5773499558827533

#Test Case-3 for tanApprox
tan(pi/2) = 1.633123935319537e16
tanApprox (pi/2) 0.001 = Infinity and tanApprox (-pi/2) 0.001 = Infinity
tanApprox (pi/2) 0.000001 = Infinity and tanApprox (-pi/2) 0.000001 = Infinity

#Test Case-4 for tanApprox
tan(pi) = -1.2246467991473532e-16
tanApprox (pi) 0.001 = 8.946128804954458e-4
tanApprox (pi) 0.000001 = 4.647660712304333e-7

#Test Case-5 for tanApprox
tan((5*pi)/6) = -0.5773502691896256
tanApprox ((5*pi)/6) 0.001 = -0.577170401177989
tanApprox ((5*pi)/6) 0.000001 = -0.5773506179257296

- -----------------------------------------------------------------
-}

