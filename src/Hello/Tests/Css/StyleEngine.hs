{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}




module Hello.Tests.Css.StyleEngine
  (
    testsCssStyleEngine
  )
where




import qualified Data.Text as T
import qualified Data.Sequence as S
import Test.HUnit

import Hello.Css.Distance
import Hello.Css.Parser
import Hello.Css.StyleEngine
import Hello.Css.Tokenizer

import Hello.Dw.FontAttrs

import Hello.Preferences
import Hello.Utils




data ValueTestType = ValueTestType
  {
    dist           :: CssDistance
  , fontAttrs      :: FontAttrs
  , referenceValue :: Int
  , dpiX           :: Float
  , dpiY           :: Float
  , ret            :: Maybe Float
  } deriving (Show)




-- This test data has been generated from debug printfs() in C++
-- StyleEngine::computeAbsoluteLengthValue() function. There is no reference
-- formula behind it, the data just relies on behaviour of (slightly
-- refactored) dillo code.
computeAbsoluteLengthValueTestData =
  [
    ValueTestType { dist = CssDistanceRelEm 0.500000,     fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 7.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.099854,     fontAttrs = makeFontAttrs 11 6,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 1.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.099854,     fontAttrs = makeFontAttrs 12 7,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 1.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.099854,     fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 1.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.199951,     fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 3.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.199951,     fontAttrs = makeFontAttrs 28 15, referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 6.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.299805,     fontAttrs = makeFontAttrs 10 6,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 3.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.299805,     fontAttrs = makeFontAttrs 11 6,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 3.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.299805,     fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 4.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.299805,     fontAttrs = makeFontAttrs 25 14, referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 7.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.399902,     fontAttrs = makeFontAttrs 10 6,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 4.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.399902,     fontAttrs = makeFontAttrs 11 6,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 4.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.399902,     fontAttrs = makeFontAttrs 12 7,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 5.0    }
  , ValueTestType { dist = CssDistanceRelEm (-0.400146),  fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just (-6.0) }
  , ValueTestType { dist = CssDistanceRelEm (-0.430176),  fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just (-6.0) }
  , ValueTestType { dist = CssDistanceRelEm 0.500000,     fontAttrs = makeFontAttrs 10 6,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 5.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.500000,     fontAttrs = makeFontAttrs 11 6,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 6.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.500000,     fontAttrs = makeFontAttrs 12 7,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 6.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.500000,     fontAttrs = makeFontAttrs 13 7,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 7.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.500000,     fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 7.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.500000,     fontAttrs = makeFontAttrs 17 10, referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 9.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.599854,     fontAttrs = makeFontAttrs 11 6,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 7.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.599854,     fontAttrs = makeFontAttrs 12 7,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 7.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.599854,     fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 8.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.669922,     fontAttrs = makeFontAttrs 28 15, referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 19.0   }
  , ValueTestType { dist = CssDistanceRelEm 0.669922,     fontAttrs = makeFontAttrs 28 16, referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 19.0   }
  , ValueTestType { dist = CssDistanceRelEm 0.750000,     fontAttrs = makeFontAttrs 21 12, referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 16.0   }
  , ValueTestType { dist = CssDistanceRelEm 0.799805,     fontAttrs = makeFontAttrs 11 6,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 9.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.799805,     fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 11.0   }
  , ValueTestType { dist = CssDistanceRelEm 0.829834,     fontAttrs = makeFontAttrs 12 7,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 10.0   }
  , ValueTestType { dist = CssDistanceRelEm 0.829834,     fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 12.0   }
  , ValueTestType { dist = CssDistanceRelEm 0.829834,     fontAttrs = makeFontAttrs 16 9,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 13.0   }
  , ValueTestType { dist = CssDistanceRelEm 0.875000,     fontAttrs = makeFontAttrs 11 6,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 10.0   }
  , ValueTestType { dist = CssDistanceRelEm 1.000000,     fontAttrs = makeFontAttrs 10 6,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 10.0   }
  , ValueTestType { dist = CssDistanceRelEm 1.000000,     fontAttrs = makeFontAttrs 11 6,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 11.0   }
  , ValueTestType { dist = CssDistanceRelEm 1.000000,     fontAttrs = makeFontAttrs 12 7,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 12.0   }
  , ValueTestType { dist = CssDistanceRelEm 1.000000,     fontAttrs = makeFontAttrs 13 7,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 13.0   }
  , ValueTestType { dist = CssDistanceRelEm 1.000000,     fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 14.0   }
  , ValueTestType { dist = CssDistanceRelEm 10.500000,    fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 147.0  }
  , ValueTestType { dist = CssDistanceRelEm 1.119873,     fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 16.0   }
  , ValueTestType { dist = CssDistanceRelEm 1.299805,     fontAttrs = makeFontAttrs 11 6,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 14.0   }
  , ValueTestType { dist = CssDistanceRelEm 1.399902,     fontAttrs = makeFontAttrs 11 6,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 15.0   }
  , ValueTestType { dist = CssDistanceRelEm 15.000000,    fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 210.0  }
  , ValueTestType { dist = CssDistanceRelEm 1.500000,     fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 21.0   }
  , ValueTestType { dist = CssDistanceRelEm 15.599854,    fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 218.0  }
  , ValueTestType { dist = CssDistanceRelEm 2.000000,     fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 28.0   }
  , ValueTestType { dist = CssDistanceRelEm 5.000000,     fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 70.0   }
  , ValueTestType { dist = CssDistanceRelEm 7.199951,     fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 101.0  }
  , ValueTestType { dist = CssDistanceRelEm 7.799805,     fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 109.0  }
  , ValueTestType { dist = CssDistanceRelEm 9.599854,     fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 134.0  }

  , ValueTestType { dist = CssNumericNone 0.000000,       fontAttrs = makeFontAttrs 10 6,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 0.0    }
  , ValueTestType { dist = CssNumericNone 0.000000,       fontAttrs = makeFontAttrs 11 6,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 0.0    }
  , ValueTestType { dist = CssNumericNone 0.000000,       fontAttrs = makeFontAttrs 11 6,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 0.0    }
  , ValueTestType { dist = CssNumericNone 0.000000,       fontAttrs = makeFontAttrs 12 7,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 0.0    }
  , ValueTestType { dist = CssNumericNone 0.000000,       fontAttrs = makeFontAttrs 13 7,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 0.0    }
  , ValueTestType { dist = CssNumericNone 0.000000,       fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 0.0    }
  , ValueTestType { dist = CssNumericNone 0.000000,       fontAttrs = makeFontAttrs 16 9,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 0.0    }
  , ValueTestType { dist = CssNumericNone 0.000000,       fontAttrs = makeFontAttrs 17 10, referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 0.0    }
  , ValueTestType { dist = CssNumericNone 0.000000,       fontAttrs = makeFontAttrs 21 12, referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 0.0    }
  , ValueTestType { dist = CssNumericNone 0.000000,       fontAttrs = makeFontAttrs 28 15, referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 0.0    }
  , ValueTestType { dist = CssNumericNone 0.000000,       fontAttrs = makeFontAttrs 28 16, referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 0.0    }

  , ValueTestType { dist = CssDistanceAbsPx 0.000000,     fontAttrs = makeFontAttrs 11 6,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 0.0    }
  , ValueTestType { dist = CssDistanceAbsPx 0.000000,     fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 0.0    }
  , ValueTestType { dist = CssDistanceAbsPx 0.000000,     fontAttrs = makeFontAttrs 17 9,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 0.0    }
  , ValueTestType { dist = CssDistanceAbsPx 0.000000,     fontAttrs = makeFontAttrs 28 15, referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 0.0    }
  , ValueTestType { dist = CssDistanceAbsPx 1.000000,     fontAttrs = makeFontAttrs 10 6,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 1.0    }
  , ValueTestType { dist = CssDistanceAbsPx 1.000000,     fontAttrs = makeFontAttrs 11 6,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 1.0    }
  , ValueTestType { dist = CssDistanceAbsPx 1.000000,     fontAttrs = makeFontAttrs 12 7,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 1.0    }
  , ValueTestType { dist = CssDistanceAbsPx 1.000000,     fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 1.0    }
  , ValueTestType { dist = CssDistanceAbsPx 11.000000,    fontAttrs = makeFontAttrs 10 6,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 11.0   }
  , ValueTestType { dist = CssDistanceAbsPx 11.000000,    fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 11.0   }
  , ValueTestType { dist = CssDistanceAbsPx 115.000000,   fontAttrs = makeFontAttrs 28 16, referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 115.0  }
  , ValueTestType { dist = CssDistanceAbsPx 120.000000,   fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 120.0  }
  , ValueTestType { dist = CssDistanceAbsPx 14.000000,    fontAttrs = makeFontAttrs 10 6,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 14.0   }
  , ValueTestType { dist = CssDistanceAbsPx 14.000000,    fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 14.0   }
  , ValueTestType { dist = CssDistanceAbsPx 15.000000,    fontAttrs = makeFontAttrs 10 6,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 15.0   }
  , ValueTestType { dist = CssDistanceAbsPx 15.000000,    fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 15.0   }
  , ValueTestType { dist = CssDistanceAbsPx 18.000000,    fontAttrs = makeFontAttrs 11 6,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 18.0   }
  , ValueTestType { dist = CssDistanceAbsPx 18.000000,    fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 18.0   }
  , ValueTestType { dist = CssDistanceAbsPx 2.000000,     fontAttrs = makeFontAttrs 11 6,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 2.0    }
  , ValueTestType { dist = CssDistanceAbsPx 2.000000,     fontAttrs = makeFontAttrs 12 7,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 2.0    }
  , ValueTestType { dist = CssDistanceAbsPx 2.000000,     fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 2.0    }
  , ValueTestType { dist = CssDistanceAbsPx 23.000000,    fontAttrs = makeFontAttrs 11 6,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 23.0   }
  , ValueTestType { dist = CssDistanceAbsPx 23.000000,    fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 23.0   }
  , ValueTestType { dist = CssDistanceAbsPx 3.000000,     fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 3.0    }
  , ValueTestType { dist = CssDistanceAbsPx 36.000000,    fontAttrs = makeFontAttrs 10 6,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 36.0   }
  , ValueTestType { dist = CssDistanceAbsPx 36.000000,    fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 36.0   }
  , ValueTestType { dist = CssDistanceAbsPx 40.000000,    fontAttrs = makeFontAttrs 12 7,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 40.0   }
  , ValueTestType { dist = CssDistanceAbsPx 40.000000,    fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 40.0   }
  , ValueTestType { dist = CssDistanceAbsPx 4.000000,     fontAttrs = makeFontAttrs 11 6,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 4.0    }
  , ValueTestType { dist = CssDistanceAbsPx 4.000000,     fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 4.0    }
  , ValueTestType { dist = CssDistanceAbsPx 5.000000,     fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 5.0    }
  , ValueTestType { dist = CssDistanceAbsPx 5.000000,     fontAttrs = makeFontAttrs 17 9,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 5.0    }
  , ValueTestType { dist = CssDistanceAbsPx 60.000000,    fontAttrs = makeFontAttrs 25 14, referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 60.0   }
  , ValueTestType { dist = CssDistanceAbsPx 6.000000,     fontAttrs = makeFontAttrs 11 6,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 6.0    }
  , ValueTestType { dist = CssDistanceAbsPx 6.000000,     fontAttrs = makeFontAttrs 14 8,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 6.0    }
  , ValueTestType { dist = CssDistanceAbsPx 6.000000,     fontAttrs = makeFontAttrs 17 9,  referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 6.0    }
  , ValueTestType { dist = CssDistanceAbsPx 80.000000,    fontAttrs = makeFontAttrs 28 16, referenceValue = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 80.0   }

  , ValueTestType { dist = CssNumericPercentage 0.799805, fontAttrs = makeFontAttrs 14 8,  referenceValue = 14, dpiX = 141.767441, dpiY = 141.402069, ret = Just 11.0   }
  , ValueTestType { dist = CssNumericPercentage 0.849854, fontAttrs = makeFontAttrs 14 8,  referenceValue = 14, dpiX = 141.767441, dpiY = 141.402069, ret = Just 12.0   }
  , ValueTestType { dist = CssNumericPercentage 0.899902, fontAttrs = makeFontAttrs 11 6,  referenceValue = 11, dpiX = 141.767441, dpiY = 141.402069, ret = Just 10.0   }
  , ValueTestType { dist = CssNumericPercentage 0.899902, fontAttrs = makeFontAttrs 14 8,  referenceValue = 14, dpiX = 141.767441, dpiY = 141.402069, ret = Just 13.0   }
  , ValueTestType { dist = CssNumericPercentage 0.949951, fontAttrs = makeFontAttrs 14 8,  referenceValue = 14, dpiX = 141.767441, dpiY = 141.402069, ret = Just 13.0   }
  , ValueTestType { dist = CssNumericPercentage 1.000000, fontAttrs = makeFontAttrs 10 6,  referenceValue = 10, dpiX = 141.767441, dpiY = 141.402069, ret = Just 10.0   }
  , ValueTestType { dist = CssNumericPercentage 1.000000, fontAttrs = makeFontAttrs 11 6,  referenceValue = 11, dpiX = 141.767441, dpiY = 141.402069, ret = Just 11.0   }
  , ValueTestType { dist = CssNumericPercentage 1.000000, fontAttrs = makeFontAttrs 12 7,  referenceValue = 12, dpiX = 141.767441, dpiY = 141.402069, ret = Just 12.0   }
  , ValueTestType { dist = CssNumericPercentage 1.000000, fontAttrs = makeFontAttrs 14 8,  referenceValue = 14, dpiX = 141.767441, dpiY = 141.402069, ret = Just 14.0   }
  , ValueTestType { dist = CssNumericPercentage 1.000000, fontAttrs = makeFontAttrs 6  4,  referenceValue = 6,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 6.00   }
  , ValueTestType { dist = CssNumericPercentage 1.099854, fontAttrs = makeFontAttrs 11 6,  referenceValue = 11, dpiX = 141.767441, dpiY = 141.402069, ret = Just 12.0   }
  , ValueTestType { dist = CssNumericPercentage 1.099854, fontAttrs = makeFontAttrs 14 8,  referenceValue = 14, dpiX = 141.767441, dpiY = 141.402069, ret = Just 15.0   }
  , ValueTestType { dist = CssNumericPercentage 1.199951, fontAttrs = makeFontAttrs 11 6,  referenceValue = 11, dpiX = 141.767441, dpiY = 141.402069, ret = Just 13.0   }
  , ValueTestType { dist = CssNumericPercentage 1.199951, fontAttrs = makeFontAttrs 14 8,  referenceValue = 14, dpiX = 141.767441, dpiY = 141.402069, ret = Just 17.0   }
  , ValueTestType { dist = CssNumericPercentage 1.619873, fontAttrs = makeFontAttrs 14 8,  referenceValue = 14, dpiX = 141.767441, dpiY = 141.402069, ret = Just 23.0   }
  , ValueTestType { dist = CssNumericPercentage 1.799805, fontAttrs = makeFontAttrs 14 8,  referenceValue = 14, dpiX = 141.767441, dpiY = 141.402069, ret = Just 25.0   }
  ]




makeFontAttrs size xHeight = defaultFontAttrs { fontSize = size, fontXHeight = xHeight }




-- On success return empty string. On failure return string representation of
-- remainder string in a row, for which test failed.
computeAbsoluteLengthValueTest :: [ValueTestType] -> Maybe T.Text
computeAbsoluteLengthValueTest []     = Nothing
computeAbsoluteLengthValueTest (x:xs) = if expected /= styleEngineComputeAbsoluteLengthValue (dist x) (fontAttrs x) (referenceValue x) (dpiX x) (dpiY x)
                                        then Just . T.pack . show $ x
                                        else computeAbsoluteLengthValueTest xs
  where
    expected = ret x




-------------------------------------------------------




data ApplyToFontTestData = ApplyToFontTestData
  {
    testDeclSet :: CssDeclarationSet
  , testPrefs   :: Preferences
  , testDpiX    :: Float
  , testDpiY    :: Float
  , testParentFontAttrs :: FontAttrs
  , testFontAttrs       :: FontAttrs
  , testOutFontAttrs    :: FontAttrs
  } deriving (Show)




styleEngineApplyStyleToFontTestData =
  [
    -- weight
    ApplyToFontTestData
    {
      testDeclSet = CssDeclarationSet {isSafe = False, items = S.fromList
                                                               [
                                                                 CssDeclWrapper {property = 29, declValue = CssValueTypeEnum 0, important = False}
                                                               , CssDeclWrapper {property = 49, declValue = CssValueTypeSignedLength (CssDistanceRelEm 0.5), important = False}
                                                               , CssDeclWrapper {property = 48, declValue = CssValueTypeSignedLength (CssNumericNone 0.0), important = False}
                                                               , CssDeclWrapper {property = 46, declValue = CssValueTypeSignedLength (CssDistanceRelEm 0.5), important = False}
                                                               , CssDeclWrapper {property = 47, declValue = CssValueTypeSignedLength (CssNumericNone 0.0), important = False}
                                                               , CssDeclWrapper {property = 38, declValue = CssValueTypeEnum 0, important = False}]} -- font weight, 0 == bold
    , testPrefs = Preferences { prefsFontSerif = "DejaVu Serif"
                              , prefsFontSansSerif = "DejaVu Sans"
                              , prefsFontCursive = "URW Chancery L"
                              , prefsFontFantasy = "DejaVu Sans"
                              , prefsFontMonospace = "DejaVu Sans Mono"
                              , prefsFontFactor = 1.0
                              , prefsFontMinSize = 6
                              , prefsFontMaxSize = 100}
    , testDpiX = 141.76744
    , testDpiY = 141.40207
    , testParentFontAttrs = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testFontAttrs       = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testOutFontAttrs    = FontAttrs {fontSize = 14, fontWeight = 700, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    }
  ,
    -- variant
    ApplyToFontTestData
    {
      testDeclSet = CssDeclarationSet {isSafe = False, items = S.fromList
                                                               [
                                                                 CssDeclWrapper {property = 29, declValue = CssValueTypeEnum 0, important = False}
                                                               , CssDeclWrapper {property = 49, declValue = CssValueTypeSignedLength (CssDistanceRelEm 0.5), important = False}
                                                               , CssDeclWrapper {property = 48, declValue = CssValueTypeSignedLength (CssNumericNone 0.0), important = False}
                                                               , CssDeclWrapper {property = 46, declValue = CssValueTypeSignedLength (CssDistanceRelEm 0.5), important = False}
                                                               , CssDeclWrapper {property = 47, declValue = CssValueTypeSignedLength (CssNumericNone 0.0), important = False}
                                                               , CssDeclWrapper {property = 37, declValue = CssValueTypeEnum 1, important = False}]} -- font variant
    , testPrefs = Preferences { prefsFontSerif = "DejaVu Serif"
                              , prefsFontSansSerif = "DejaVu Sans"
                              , prefsFontCursive = "URW Chancery L"
                              , prefsFontFantasy = "DejaVu Sans"
                              , prefsFontMonospace = "DejaVu Sans Mono"
                              , prefsFontFactor = 1.0
                              , prefsFontMinSize = 6
                              , prefsFontMaxSize = 100}
    , testDpiX = 141.76744
    , testDpiY = 141.40207
    , testParentFontAttrs = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testFontAttrs       = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testOutFontAttrs    = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 1, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    }
  ,
    -- family
    ApplyToFontTestData
    {
      testDeclSet = CssDeclarationSet {isSafe = False, items = S.fromList
                                                               [
                                                                 CssDeclWrapper {property = 29, declValue = CssValueTypeEnum 0, important = False}
                                                               , CssDeclWrapper {property = 49, declValue = CssValueTypeSignedLength (CssDistanceRelEm 0.5), important = False}
                                                               , CssDeclWrapper {property = 48, declValue = CssValueTypeSignedLength (CssNumericNone 0.0), important = False}
                                                               , CssDeclWrapper {property = 46, declValue = CssValueTypeSignedLength (CssDistanceRelEm 0.5), important = False}
                                                               , CssDeclWrapper {property = 47, declValue = CssValueTypeSignedLength (CssNumericNone 0.0), important = False}
                                                               , CssDeclWrapper {property = 32, declValue = CssValueTypeStringList ["serif"], important = False}]} -- font family
    , testPrefs = Preferences { prefsFontSerif = "DejaVu Serif"
                              , prefsFontSansSerif = "DejaVu Sans"
                              , prefsFontCursive = "URW Chancery L"
                              , prefsFontFantasy = "DejaVu Sans"
                              , prefsFontMonospace = "DejaVu Sans Mono"
                              , prefsFontFactor = 1.0
                              , prefsFontMinSize = 6
                              , prefsFontMaxSize = 100}
    , testDpiX = 141.76744
    , testDpiY = 141.40207
    , testParentFontAttrs = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testFontAttrs       = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testOutFontAttrs    = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Serif", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    }
  ,
    -- size, with standard font factor
    ApplyToFontTestData
    {
      testDeclSet = CssDeclarationSet {isSafe = False, items = S.fromList
                                                               [
                                                                 CssDeclWrapper {property = 29, declValue = CssValueTypeEnum 0, important = False}
                                                               , CssDeclWrapper {property = 49, declValue = CssValueTypeSignedLength (CssDistanceRelEm 0.5), important = False}
                                                               , CssDeclWrapper {property = 48, declValue = CssValueTypeSignedLength (CssNumericNone 0.0), important = False}
                                                               , CssDeclWrapper {property = 46, declValue = CssValueTypeSignedLength (CssDistanceRelEm 0.5), important = False}
                                                               , CssDeclWrapper {property = 47, declValue = CssValueTypeSignedLength (CssNumericNone 0.0), important = False}
                                                               , CssDeclWrapper {property = 33, declValue = CssValueTypeEnum 6, important = False}]} -- font size, 6 == xx-small, 8.1 * prefs.prefsFontFactor
    , testPrefs = Preferences { prefsFontSerif = "DejaVu Serif"
                              , prefsFontSansSerif = "DejaVu Sans"
                              , prefsFontCursive = "URW Chancery L"
                              , prefsFontFantasy = "DejaVu Sans"
                              , prefsFontMonospace = "DejaVu Sans Mono"
                              , prefsFontFactor = 1.0
                              , prefsFontMinSize = 6
                              , prefsFontMaxSize = 100}
    , testDpiX = 141.76744
    , testDpiY = 141.40207
    , testParentFontAttrs = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testFontAttrs       = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testOutFontAttrs    = FontAttrs {fontSize = 8, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    }
  ,
    -- size, with increased font factor
    ApplyToFontTestData
    {
      testDeclSet = CssDeclarationSet {isSafe = False, items = S.fromList
                                                               [
                                                                 CssDeclWrapper {property = 29, declValue = CssValueTypeEnum 0, important = False}
                                                               , CssDeclWrapper {property = 49, declValue = CssValueTypeSignedLength (CssDistanceRelEm 0.5), important = False}
                                                               , CssDeclWrapper {property = 48, declValue = CssValueTypeSignedLength (CssNumericNone 0.0), important = False}
                                                               , CssDeclWrapper {property = 46, declValue = CssValueTypeSignedLength (CssDistanceRelEm 0.5), important = False}
                                                               , CssDeclWrapper {property = 47, declValue = CssValueTypeSignedLength (CssNumericNone 0.0), important = False}
                                                               , CssDeclWrapper {property = 33, declValue = CssValueTypeEnum 6, important = False}]} -- font size, 6 == xx-small, 8.1 * prefs.prefsFontFactor
    , testPrefs = Preferences { prefsFontSerif = "DejaVu Serif"
                              , prefsFontSansSerif = "DejaVu Sans"
                              , prefsFontCursive = "URW Chancery L"
                              , prefsFontFantasy = "DejaVu Sans"
                              , prefsFontMonospace = "DejaVu Sans Mono"
                              , prefsFontFactor = 2.0 -- increased from 2.0
                              , prefsFontMinSize = 6
                              , prefsFontMaxSize = 100}
    , testDpiX = 141.76744
    , testDpiY = 141.40207
    , testParentFontAttrs = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testFontAttrs       = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testOutFontAttrs    = FontAttrs {fontSize = 16, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    }
  ,
    -- size, clipped from bottom (prefxFontMinSize)
    ApplyToFontTestData
    {
      testDeclSet = CssDeclarationSet {isSafe = False, items = S.fromList
                                                               [
                                                                 CssDeclWrapper {property = 29, declValue = CssValueTypeEnum 0, important = False}
                                                               , CssDeclWrapper {property = 49, declValue = CssValueTypeSignedLength (CssDistanceRelEm 0.5), important = False}
                                                               , CssDeclWrapper {property = 48, declValue = CssValueTypeSignedLength (CssNumericNone 0.0), important = False}
                                                               , CssDeclWrapper {property = 46, declValue = CssValueTypeSignedLength (CssDistanceRelEm 0.5), important = False}
                                                               , CssDeclWrapper {property = 47, declValue = CssValueTypeSignedLength (CssNumericNone 0.0), important = False}
                                                               , CssDeclWrapper {property = 33, declValue = CssValueTypeEnum 6, important = False}]} -- font size, 6 == xx-small, 8.1 * prefs.prefsFontFactor
    , testPrefs = Preferences { prefsFontSerif = "DejaVu Serif"
                              , prefsFontSansSerif = "DejaVu Sans"
                              , prefsFontCursive = "URW Chancery L"
                              , prefsFontFantasy = "DejaVu Sans"
                              , prefsFontMonospace = "DejaVu Sans Mono"
                              , prefsFontFactor = 1.0
                              , prefsFontMinSize = 12  -- this will clip value from 8 to 12
                              , prefsFontMaxSize = 100}
    , testDpiX = 141.76744
    , testDpiY = 141.40207
    , testParentFontAttrs = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testFontAttrs       = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testOutFontAttrs    = FontAttrs {fontSize = 12, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    }
  ,
    -- size, clipped from top (prefxFontMaxSize)
    ApplyToFontTestData
    {
      testDeclSet = CssDeclarationSet {isSafe = False, items = S.fromList
                                                               [
                                                                 CssDeclWrapper {property = 29, declValue = CssValueTypeEnum 0, important = False}
                                                               , CssDeclWrapper {property = 49, declValue = CssValueTypeSignedLength (CssDistanceRelEm 0.5), important = False}
                                                               , CssDeclWrapper {property = 48, declValue = CssValueTypeSignedLength (CssNumericNone 0.0), important = False}
                                                               , CssDeclWrapper {property = 46, declValue = CssValueTypeSignedLength (CssDistanceRelEm 0.5), important = False}
                                                               , CssDeclWrapper {property = 47, declValue = CssValueTypeSignedLength (CssNumericNone 0.0), important = False}
                                                               , CssDeclWrapper {property = 33, declValue = CssValueTypeEnum 5, important = False}]} -- font size, 5 == xx-large, 24.2 * prefs.prefsFontFactor
    , testPrefs = Preferences { prefsFontSerif = "DejaVu Serif"
                              , prefsFontSansSerif = "DejaVu Sans"
                              , prefsFontCursive = "URW Chancery L"
                              , prefsFontFantasy = "DejaVu Sans"
                              , prefsFontMonospace = "DejaVu Sans Mono"
                              , prefsFontFactor = 1.0
                              , prefsFontMinSize = 6
                              , prefsFontMaxSize = 17} -- this will clip value from 24 to 17
    , testDpiX = 141.76744
    , testDpiY = 141.40207
    , testParentFontAttrs = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testFontAttrs       = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testOutFontAttrs    = FontAttrs {fontSize = 17, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    }
  ,



    -- letter spacing
    ApplyToFontTestData
    {
      testDeclSet = CssDeclarationSet {isSafe = False, items = S.fromList
                                                               [
                                                                 CssDeclWrapper {property = 29, declValue = CssValueTypeEnum 0, important = False}
                                                               , CssDeclWrapper {property = 49, declValue = CssValueTypeSignedLength (CssDistanceRelEm 0.5), important = False}
                                                               , CssDeclWrapper {property = 48, declValue = CssValueTypeSignedLength (CssNumericNone 0.0), important = False}
                                                               , CssDeclWrapper {property = 46, declValue = CssValueTypeSignedLength (CssDistanceRelEm 0.5), important = False}
                                                               , CssDeclWrapper {property = 47, declValue = CssValueTypeSignedLength (CssNumericNone 0.0), important = False}
                                                               , CssDeclWrapper {property = 41, declValue = CssValueTypeSignedLength (CssDistanceAbsPx 5.0), important = False}]} -- letter spacing
    , testPrefs = Preferences { prefsFontSerif = "DejaVu Serif"
                              , prefsFontSansSerif = "DejaVu Sans"
                              , prefsFontCursive = "URW Chancery L"
                              , prefsFontFantasy = "DejaVu Sans"
                              , prefsFontMonospace = "DejaVu Sans Mono"
                              , prefsFontFactor = 1.0
                              , prefsFontMinSize = 6
                              , prefsFontMaxSize = 100}
    , testDpiX = 141.76744
    , testDpiY = 141.40207
    , testParentFontAttrs = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testFontAttrs       = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testOutFontAttrs    = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 5}
    }
    ,
    ApplyToFontTestData
    {
      testDeclSet = CssDeclarationSet {isSafe = False, items = S.fromList
                                                               [
                                                                 CssDeclWrapper {property = 41, declValue = CssValueTypeSignedLength (CssDistanceAbsPx 2.0), important = False}
                                                               , CssDeclWrapper {property = 29, declValue = CssValueTypeEnum 0, important = False}
                                                               , CssDeclWrapper {property = 49, declValue = CssValueTypeSignedLength (CssDistanceRelEm 0.5), important = False}
                                                               , CssDeclWrapper {property = 48, declValue = CssValueTypeSignedLength (CssNumericNone 0.0), important = False}
                                                               , CssDeclWrapper {property = 46, declValue = CssValueTypeSignedLength (CssDistanceRelEm 0.5), important = False}
                                                               , CssDeclWrapper {property = 47, declValue = CssValueTypeSignedLength (CssNumericNone 0.0), important = False}]} -- letter spacing
    , testPrefs = Preferences { prefsFontSerif = "DejaVu Serif"
                              , prefsFontSansSerif = "DejaVu Sans"
                              , prefsFontCursive = "URW Chancery L"
                              , prefsFontFantasy = "DejaVu Sans"
                              , prefsFontMonospace = "DejaVu Sans Mono"
                              , prefsFontFactor = 1.0
                              , prefsFontMinSize = 6
                              , prefsFontMaxSize = 100}
    , testDpiX = 141.76744
    , testDpiY = 141.40207
    , testParentFontAttrs = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testFontAttrs       = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testOutFontAttrs    = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 2}
    }
    ,



    -- font style
    ApplyToFontTestData
    {
      testDeclSet = CssDeclarationSet {isSafe = False, items = S.fromList
                                                               [
                                                                 CssDeclWrapper {property = 29, declValue = CssValueTypeEnum 0, important = False}
                                                               , CssDeclWrapper {property = 49, declValue = CssValueTypeSignedLength (CssDistanceRelEm 0.5), important = False}
                                                               , CssDeclWrapper {property = 48, declValue = CssValueTypeSignedLength (CssNumericNone 0.0), important = False}
                                                               , CssDeclWrapper {property = 46, declValue = CssValueTypeSignedLength (CssDistanceRelEm 0.5), important = False}
                                                               , CssDeclWrapper {property = 47, declValue = CssValueTypeSignedLength (CssNumericNone 0.0), important = False}
                                                               , CssDeclWrapper {property = 36, declValue = CssValueTypeEnum 2, important = False}]} -- font style
    , testPrefs = Preferences { prefsFontSerif = "DejaVu Serif"
                              , prefsFontSansSerif = "DejaVu Sans"
                              , prefsFontCursive = "URW Chancery L"
                              , prefsFontFantasy = "DejaVu Sans"
                              , prefsFontMonospace = "DejaVu Sans Mono"
                              , prefsFontFactor = 1.0
                              , prefsFontMinSize = 6
                              , prefsFontMaxSize = 100}
    , testDpiX = 141.76744
    , testDpiY = 141.40207
    , testParentFontAttrs = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testFontAttrs       = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testOutFontAttrs    = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 2, fontXHeight = 8, fontLetterSpacing = 0}
    }
    ,



    -- Just some random tests from some webpage



    ApplyToFontTestData
    {
      testDeclSet = CssDeclarationSet {isSafe = True, items = S.fromList [CssDeclWrapper {property = 38, declValue = CssValueTypeEnum 1, important = False}]} -- font weight, 1 == bolder
    , testPrefs = Preferences { prefsFontSerif = "DejaVu Serif"
                              , prefsFontSansSerif = "DejaVu Sans"
                              , prefsFontCursive = "URW Chancery L"
                              , prefsFontFantasy = "DejaVu Sans"
                              , prefsFontMonospace = "DejaVu Sans Mono"
                              , prefsFontFactor = 1.0
                              , prefsFontMinSize = 6
                              , prefsFontMaxSize = 100}
    , testDpiX = 141.76744
    , testDpiY = 141.40207
    , testParentFontAttrs = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testFontAttrs       = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testOutFontAttrs    = FontAttrs {fontSize = 14, fontWeight = 700, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    }
  ,
    ApplyToFontTestData
    {
      testDeclSet = CssDeclarationSet {isSafe = False, items = S.fromList
                                                               [
                                                                 CssDeclWrapper {property = 38, declValue = CssValueTypeEnum 1, important = False} -- font weight, 1 == bolder
                                                               , CssDeclWrapper {property = 29, declValue = CssValueTypeEnum 0, important = False}
                                                               , CssDeclWrapper {property = 33, declValue = CssValueTypeLengthPercent (CssDistanceRelEm 2.0), important = False} -- font size
                                                               , CssDeclWrapper {property = 49, declValue = CssValueTypeSignedLength (CssDistanceRelEm 0.67), important = False}
                                                               , CssDeclWrapper {property = 46, declValue = CssValueTypeSignedLength (CssNumericNone 0.0), important = False}]}
    , testPrefs = Preferences { prefsFontSerif = "DejaVu Serif"
                              , prefsFontSansSerif = "DejaVu Sans"
                              , prefsFontCursive = "URW Chancery L"
                              , prefsFontFantasy = "DejaVu Sans"
                              , prefsFontMonospace = "DejaVu Sans Mono"
                              , prefsFontFactor = 1.0
                              , prefsFontMinSize = 6
                              , prefsFontMaxSize = 100}
    , testDpiX = 141.76744
    , testDpiY = 141.40207
    , testParentFontAttrs = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testFontAttrs       = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testOutFontAttrs    = FontAttrs {fontSize = 28, fontWeight = 700, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    }
  ,
    ApplyToFontTestData
    {
      testDeclSet = CssDeclarationSet {isSafe = False, items = S.fromList
                                                               [
                                                                 CssDeclWrapper {property = 38, declValue = CssValueTypeEnum 1, important = False} -- font weight, 1 == bolder
                                                               , CssDeclWrapper {property = 29, declValue = CssValueTypeEnum 0, important = False}
                                                               , CssDeclWrapper {property = 33, declValue = CssValueTypeLengthPercent (CssDistanceRelEm 1.5), important = False} -- font size
                                                               , CssDeclWrapper {property = 49, declValue = CssValueTypeSignedLength (CssDistanceRelEm 0.75), important = False}
                                                               , CssDeclWrapper {property = 46, declValue = CssValueTypeSignedLength (CssNumericNone 0.0), important = False}]}
    , testPrefs = Preferences { prefsFontSerif = "DejaVu Serif"
                              , prefsFontSansSerif = "DejaVu Sans"
                              , prefsFontCursive = "URW Chancery L"
                              , prefsFontFantasy = "DejaVu Sans"
                              , prefsFontMonospace = "DejaVu Sans Mono"
                              , prefsFontFactor = 1.0
                              , prefsFontMinSize = 6
                              , prefsFontMaxSize = 100}
    , testDpiX = 141.76744
    , testDpiY = 141.40207
    , testParentFontAttrs = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testFontAttrs       = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testOutFontAttrs    = FontAttrs {fontSize = 21, fontWeight = 700, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    }
  ,
    ApplyToFontTestData
    {
      testDeclSet = CssDeclarationSet {isSafe = True, items = S.fromList [CssDeclWrapper {property = 32, declValue = CssValueTypeStringList ["monospace"], important = False}]} -- font family
    , testPrefs = Preferences { prefsFontSerif = "DejaVu Serif"
                              , prefsFontSansSerif = "DejaVu Sans"
                              , prefsFontCursive = "URW Chancery L"
                              , prefsFontFantasy = "DejaVu Sans"
                              , prefsFontMonospace = "DejaVu Sans Mono"
                              , prefsFontFactor = 1.0
                              , prefsFontMinSize = 6
                              , prefsFontMaxSize = 100}
    , testDpiX = 141.76744
    , testDpiY = 141.40207
    , testParentFontAttrs = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testFontAttrs       = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testOutFontAttrs    = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans Mono", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    }
  ,
    ApplyToFontTestData
    {
      testDeclSet = CssDeclarationSet {isSafe = True, items = S.fromList
                                                              [
                                                                CssDeclWrapper {property = 32, declValue = CssValueTypeStringList ["monospace"], important = False} -- font family
                                                              , CssDeclWrapper {property = 33, declValue = CssValueTypeLengthPercent (CssDistanceRelEm 1.5), important = False} -- font size
                                                              ]}
    , testPrefs = Preferences { prefsFontSerif = "DejaVu Serif"
                              , prefsFontSansSerif = "DejaVu Sans"
                              , prefsFontCursive = "URW Chancery L"
                              , prefsFontFantasy = "DejaVu Sans"
                              , prefsFontMonospace = "DejaVu Sans Mono"
                              , prefsFontFactor = 1.0
                              , prefsFontMinSize = 6
                              , prefsFontMaxSize = 100}
    , testDpiX = 141.76744
    , testDpiY = 141.40207
    , testParentFontAttrs = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testFontAttrs       = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testOutFontAttrs    = FontAttrs {fontSize = 21, fontWeight = 400, fontName = "DejaVu Sans Mono", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    }
  ,
    ApplyToFontTestData
    {
      testDeclSet = CssDeclarationSet {isSafe = False, items = S.fromList
                                                               [
                                                                 CssDeclWrapper {property = 38, declValue = CssValueTypeEnum 1, important = False} -- font weight, 1 == bolder
                                                               , CssDeclWrapper {property = 29, declValue = CssValueTypeEnum 0, important = False}
                                                               , CssDeclWrapper {property = 33, declValue = CssValueTypeLengthPercent (CssDistanceRelEm 1.5), important = False} -- font size
                                                               , CssDeclWrapper {property = 49, declValue = CssValueTypeSignedLength (CssDistanceRelEm 0.75), important = False}
                                                               , CssDeclWrapper {property = 46, declValue = CssValueTypeSignedLength (CssNumericNone 0.0), important = False}]}
    , testPrefs = Preferences { prefsFontSerif = "DejaVu Serif"
                              , prefsFontSansSerif = "DejaVu Sans"
                              , prefsFontCursive = "URW Chancery L"
                              , prefsFontFantasy = "DejaVu Sans"
                              , prefsFontMonospace = "DejaVu Sans Mono"
                              , prefsFontFactor = 1.0
                              , prefsFontMinSize = 6
                              , prefsFontMaxSize = 100}
    , testDpiX = 141.76744
    , testDpiY = 141.40207
    , testParentFontAttrs = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testFontAttrs       = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testOutFontAttrs    = FontAttrs {fontSize = 21, fontWeight = 700, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    }
  ,
    ApplyToFontTestData
    {
      testDeclSet = CssDeclarationSet {isSafe = False, items = S.fromList
                                                               [
                                                                 CssDeclWrapper {property = 38, declValue = CssValueTypeFontWeight 600, important = False} -- font weight, specific value
                                                               , CssDeclWrapper {property = 29, declValue = CssValueTypeEnum 0, important = False}
                                                               , CssDeclWrapper {property = 33, declValue = CssValueTypeLengthPercent (CssDistanceRelEm 1.5), important = False} -- font size
                                                               , CssDeclWrapper {property = 49, declValue = CssValueTypeSignedLength (CssDistanceRelEm 0.75), important = False}
                                                               , CssDeclWrapper {property = 46, declValue = CssValueTypeSignedLength (CssNumericNone 0.0), important = False}]}
    , testPrefs = Preferences { prefsFontSerif = "DejaVu Serif"
                              , prefsFontSansSerif = "DejaVu Sans"
                              , prefsFontCursive = "URW Chancery L"
                              , prefsFontFantasy = "DejaVu Sans"
                              , prefsFontMonospace = "DejaVu Sans Mono"
                              , prefsFontFactor = 1.0
                              , prefsFontMinSize = 6
                              , prefsFontMaxSize = 100}
    , testDpiX = 141.76744
    , testDpiY = 141.40207
    , testParentFontAttrs = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testFontAttrs       = FontAttrs {fontSize = 14, fontWeight = 400, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    , testOutFontAttrs    = FontAttrs {fontSize = 21, fontWeight = 600, fontName = "DejaVu Sans", fontVariant = 0, fontStyle = 0, fontXHeight = 8, fontLetterSpacing = 0}
    }
  ]




-- On success return empty string. On failure return string representation of
-- remainder string in a row, for which test failed.
styleEngineApplyStyleToFontTest :: [ApplyToFontTestData] -> Maybe T.Text
styleEngineApplyStyleToFontTest []     = Nothing
styleEngineApplyStyleToFontTest (x:xs) = if expected /= styleEngineApplyStyleToFont (testDeclSet x) (testPrefs x) (testDpiX x) (testDpiY x) (testParentFontAttrs x) (testFontAttrs x)
                                         then Just . T.pack . show $ x
                                         else styleEngineApplyStyleToFontTest xs
  where
    expected = testOutFontAttrs x




-------------------------------------------------------




testCases =
  [
    -- If some error is found, test function returns some data (e.g. non-empty
    -- string or test index) which can help identify which test failed.
    TestCase (do assertEqual "manual tests of css distance absolute value"            Nothing (computeAbsoluteLengthValueTest computeAbsoluteLengthValueTestData))
  , TestCase (do assertEqual "manual tests of css applyint style to font"             Nothing (styleEngineApplyStyleToFontTest styleEngineApplyStyleToFontTestData))
  ]




testsCssStyleEngine :: IO String
testsCssStyleEngine = do
  counts <- runTestTT (TestList (testCases))
  if (errors counts + failures counts == 0)
    then return ""
    else return "[EE] Hello.Tests.Css.StyleEngine failed"

