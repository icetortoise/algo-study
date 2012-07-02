import Foreign
import Foreign.C.Types

foreign import ccall unsafe "math.h sin"
        c_sin :: CDouble -> CDouble

-- |Type-conversion wrapper around c_sin
sin :: Double -> Double
-- sin x = 0.8
sin = realToFrac . c_sin . realToFrac

main = print $ Main.sin 12.4