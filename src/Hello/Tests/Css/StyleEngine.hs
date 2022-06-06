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
import Test.HUnit

import Hello.Css.Distance
import Hello.Css.Parser
import Hello.Css.StyleEngine
import Hello.Css.Tokenizer

import Hello.Utils


data ValueTestType = ValueTestType
  {
    dist :: CssDistance
  , fontSize :: Int
  , fontXHeight :: Int
  , percentageBase :: Int
  , dpiX :: Float
  , dpiY :: Float
  , ret :: Maybe Float
  } deriving (Show)




-- This test data has been generated from debug printfs() in C++
-- StyleEngine::computeAbsoluteLengthValue() function. There is no reference
-- formula behind it, the data just relies on behaviour of (slightly
-- refactored) dillo code.
computeAbsoluteLengthValueTestData =
  [
    ValueTestType { dist = CssDistanceRelEm 0.500000,     fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 7.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.099854,     fontSize = 11, fontXHeight = 6,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 1.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.099854,     fontSize = 12, fontXHeight = 7,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 1.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.099854,     fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 1.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.199951,     fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 3.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.199951,     fontSize = 28, fontXHeight = 15, percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 6.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.299805,     fontSize = 10, fontXHeight = 6,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 3.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.299805,     fontSize = 11, fontXHeight = 6,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 3.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.299805,     fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 4.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.299805,     fontSize = 25, fontXHeight = 14, percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 7.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.399902,     fontSize = 10, fontXHeight = 6,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 4.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.399902,     fontSize = 11, fontXHeight = 6,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 4.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.399902,     fontSize = 12, fontXHeight = 7,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 5.0    }
  , ValueTestType { dist = CssDistanceRelEm (-0.400146),  fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just (-6.0) }
  , ValueTestType { dist = CssDistanceRelEm (-0.430176),  fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just (-6.0) }
  , ValueTestType { dist = CssDistanceRelEm 0.500000,     fontSize = 10, fontXHeight = 6,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 5.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.500000,     fontSize = 11, fontXHeight = 6,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 6.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.500000,     fontSize = 12, fontXHeight = 7,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 6.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.500000,     fontSize = 13, fontXHeight = 7,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 7.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.500000,     fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 7.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.500000,     fontSize = 17, fontXHeight = 10, percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 9.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.599854,     fontSize = 11, fontXHeight = 6,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 7.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.599854,     fontSize = 12, fontXHeight = 7,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 7.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.599854,     fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 8.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.669922,     fontSize = 28, fontXHeight = 15, percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 19.0   }
  , ValueTestType { dist = CssDistanceRelEm 0.669922,     fontSize = 28, fontXHeight = 16, percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 19.0   }
  , ValueTestType { dist = CssDistanceRelEm 0.750000,     fontSize = 21, fontXHeight = 12, percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 16.0   }
  , ValueTestType { dist = CssDistanceRelEm 0.799805,     fontSize = 11, fontXHeight = 6,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 9.0    }
  , ValueTestType { dist = CssDistanceRelEm 0.799805,     fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 11.0   }
  , ValueTestType { dist = CssDistanceRelEm 0.829834,     fontSize = 12, fontXHeight = 7,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 10.0   }
  , ValueTestType { dist = CssDistanceRelEm 0.829834,     fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 12.0   }
  , ValueTestType { dist = CssDistanceRelEm 0.829834,     fontSize = 16, fontXHeight = 9,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 13.0   }
  , ValueTestType { dist = CssDistanceRelEm 0.875000,     fontSize = 11, fontXHeight = 6,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 10.0   }
  , ValueTestType { dist = CssDistanceRelEm 1.000000,     fontSize = 10, fontXHeight = 6,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 10.0   }
  , ValueTestType { dist = CssDistanceRelEm 1.000000,     fontSize = 11, fontXHeight = 6,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 11.0   }
  , ValueTestType { dist = CssDistanceRelEm 1.000000,     fontSize = 12, fontXHeight = 7,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 12.0   }
  , ValueTestType { dist = CssDistanceRelEm 1.000000,     fontSize = 13, fontXHeight = 7,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 13.0   }
  , ValueTestType { dist = CssDistanceRelEm 1.000000,     fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 14.0   }
  , ValueTestType { dist = CssDistanceRelEm 10.500000,    fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 147.0  }
  , ValueTestType { dist = CssDistanceRelEm 1.119873,     fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 16.0   }
  , ValueTestType { dist = CssDistanceRelEm 1.299805,     fontSize = 11, fontXHeight = 6,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 14.0   }
  , ValueTestType { dist = CssDistanceRelEm 1.399902,     fontSize = 11, fontXHeight = 6,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 15.0   }
  , ValueTestType { dist = CssDistanceRelEm 15.000000,    fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 210.0  }
  , ValueTestType { dist = CssDistanceRelEm 1.500000,     fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 21.0   }
  , ValueTestType { dist = CssDistanceRelEm 15.599854,    fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 218.0  }
  , ValueTestType { dist = CssDistanceRelEm 2.000000,     fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 28.0   }
  , ValueTestType { dist = CssDistanceRelEm 5.000000,     fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 70.0   }
  , ValueTestType { dist = CssDistanceRelEm 7.199951,     fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 101.0  }
  , ValueTestType { dist = CssDistanceRelEm 7.799805,     fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 109.0  }
  , ValueTestType { dist = CssDistanceRelEm 9.599854,     fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 134.0  }

  , ValueTestType { dist = CssNumericNone 0.000000,       fontSize = 10, fontXHeight = 6,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 0.0    }
  , ValueTestType { dist = CssNumericNone 0.000000,       fontSize = 11, fontXHeight = 6,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 0.0    }
  , ValueTestType { dist = CssNumericNone 0.000000,       fontSize = 11, fontXHeight = 6,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 0.0    }
  , ValueTestType { dist = CssNumericNone 0.000000,       fontSize = 12, fontXHeight = 7,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 0.0    }
  , ValueTestType { dist = CssNumericNone 0.000000,       fontSize = 13, fontXHeight = 7,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 0.0    }
  , ValueTestType { dist = CssNumericNone 0.000000,       fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 0.0    }
  , ValueTestType { dist = CssNumericNone 0.000000,       fontSize = 16, fontXHeight = 9,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 0.0    }
  , ValueTestType { dist = CssNumericNone 0.000000,       fontSize = 17, fontXHeight = 10, percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 0.0    }
  , ValueTestType { dist = CssNumericNone 0.000000,       fontSize = 21, fontXHeight = 12, percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 0.0    }
  , ValueTestType { dist = CssNumericNone 0.000000,       fontSize = 28, fontXHeight = 15, percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 0.0    }
  , ValueTestType { dist = CssNumericNone 0.000000,       fontSize = 28, fontXHeight = 16, percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 0.0    }

  , ValueTestType { dist = CssDistanceAbsPx 0.000000,     fontSize = 11, fontXHeight = 6,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 0.0    }
  , ValueTestType { dist = CssDistanceAbsPx 0.000000,     fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 0.0    }
  , ValueTestType { dist = CssDistanceAbsPx 0.000000,     fontSize = 17, fontXHeight = 9,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 0.0    }
  , ValueTestType { dist = CssDistanceAbsPx 0.000000,     fontSize = 28, fontXHeight = 15, percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 0.0    }
  , ValueTestType { dist = CssDistanceAbsPx 1.000000,     fontSize = 10, fontXHeight = 6,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 1.0    }
  , ValueTestType { dist = CssDistanceAbsPx 1.000000,     fontSize = 11, fontXHeight = 6,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 1.0    }
  , ValueTestType { dist = CssDistanceAbsPx 1.000000,     fontSize = 12, fontXHeight = 7,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 1.0    }
  , ValueTestType { dist = CssDistanceAbsPx 1.000000,     fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 1.0    }
  , ValueTestType { dist = CssDistanceAbsPx 11.000000,    fontSize = 10, fontXHeight = 6,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 11.0   }
  , ValueTestType { dist = CssDistanceAbsPx 11.000000,    fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 11.0   }
  , ValueTestType { dist = CssDistanceAbsPx 115.000000,   fontSize = 28, fontXHeight = 16, percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 115.0  }
  , ValueTestType { dist = CssDistanceAbsPx 120.000000,   fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 120.0  }
  , ValueTestType { dist = CssDistanceAbsPx 14.000000,    fontSize = 10, fontXHeight = 6,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 14.0   }
  , ValueTestType { dist = CssDistanceAbsPx 14.000000,    fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 14.0   }
  , ValueTestType { dist = CssDistanceAbsPx 15.000000,    fontSize = 10, fontXHeight = 6,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 15.0   }
  , ValueTestType { dist = CssDistanceAbsPx 15.000000,    fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 15.0   }
  , ValueTestType { dist = CssDistanceAbsPx 18.000000,    fontSize = 11, fontXHeight = 6,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 18.0   }
  , ValueTestType { dist = CssDistanceAbsPx 18.000000,    fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 18.0   }
  , ValueTestType { dist = CssDistanceAbsPx 2.000000,     fontSize = 11, fontXHeight = 6,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 2.0    }
  , ValueTestType { dist = CssDistanceAbsPx 2.000000,     fontSize = 12, fontXHeight = 7,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 2.0    }
  , ValueTestType { dist = CssDistanceAbsPx 2.000000,     fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 2.0    }
  , ValueTestType { dist = CssDistanceAbsPx 23.000000,    fontSize = 11, fontXHeight = 6,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 23.0   }
  , ValueTestType { dist = CssDistanceAbsPx 23.000000,    fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 23.0   }
  , ValueTestType { dist = CssDistanceAbsPx 3.000000,     fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 3.0    }
  , ValueTestType { dist = CssDistanceAbsPx 36.000000,    fontSize = 10, fontXHeight = 6,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 36.0   }
  , ValueTestType { dist = CssDistanceAbsPx 36.000000,    fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 36.0   }
  , ValueTestType { dist = CssDistanceAbsPx 40.000000,    fontSize = 12, fontXHeight = 7,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 40.0   }
  , ValueTestType { dist = CssDistanceAbsPx 40.000000,    fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 40.0   }
  , ValueTestType { dist = CssDistanceAbsPx 4.000000,     fontSize = 11, fontXHeight = 6,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 4.0    }
  , ValueTestType { dist = CssDistanceAbsPx 4.000000,     fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 4.0    }
  , ValueTestType { dist = CssDistanceAbsPx 5.000000,     fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 5.0    }
  , ValueTestType { dist = CssDistanceAbsPx 5.000000,     fontSize = 17, fontXHeight = 9,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 5.0    }
  , ValueTestType { dist = CssDistanceAbsPx 60.000000,    fontSize = 25, fontXHeight = 14, percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 60.0   }
  , ValueTestType { dist = CssDistanceAbsPx 6.000000,     fontSize = 11, fontXHeight = 6,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 6.0    }
  , ValueTestType { dist = CssDistanceAbsPx 6.000000,     fontSize = 14, fontXHeight = 8,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 6.0    }
  , ValueTestType { dist = CssDistanceAbsPx 6.000000,     fontSize = 17, fontXHeight = 9,  percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 6.0    }
  , ValueTestType { dist = CssDistanceAbsPx 80.000000,    fontSize = 28, fontXHeight = 16, percentageBase = 0,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 80.0   }

  , ValueTestType { dist = CssNumericPercentage 0.799805, fontSize = 14, fontXHeight = 8,  percentageBase = 14, dpiX = 141.767441, dpiY = 141.402069, ret = Just 11.0   }
  , ValueTestType { dist = CssNumericPercentage 0.849854, fontSize = 14, fontXHeight = 8,  percentageBase = 14, dpiX = 141.767441, dpiY = 141.402069, ret = Just 12.0   }
  , ValueTestType { dist = CssNumericPercentage 0.899902, fontSize = 11, fontXHeight = 6,  percentageBase = 11, dpiX = 141.767441, dpiY = 141.402069, ret = Just 10.0   }
  , ValueTestType { dist = CssNumericPercentage 0.899902, fontSize = 14, fontXHeight = 8,  percentageBase = 14, dpiX = 141.767441, dpiY = 141.402069, ret = Just 13.0   }
  , ValueTestType { dist = CssNumericPercentage 0.949951, fontSize = 14, fontXHeight = 8,  percentageBase = 14, dpiX = 141.767441, dpiY = 141.402069, ret = Just 13.0   }
  , ValueTestType { dist = CssNumericPercentage 1.000000, fontSize = 10, fontXHeight = 6,  percentageBase = 10, dpiX = 141.767441, dpiY = 141.402069, ret = Just 10.0   }
  , ValueTestType { dist = CssNumericPercentage 1.000000, fontSize = 11, fontXHeight = 6,  percentageBase = 11, dpiX = 141.767441, dpiY = 141.402069, ret = Just 11.0   }
  , ValueTestType { dist = CssNumericPercentage 1.000000, fontSize = 12, fontXHeight = 7,  percentageBase = 12, dpiX = 141.767441, dpiY = 141.402069, ret = Just 12.0   }
  , ValueTestType { dist = CssNumericPercentage 1.000000, fontSize = 14, fontXHeight = 8,  percentageBase = 14, dpiX = 141.767441, dpiY = 141.402069, ret = Just 14.0   }
  , ValueTestType { dist = CssNumericPercentage 1.000000, fontSize = 6,  fontXHeight = 4,  percentageBase = 6,  dpiX = 141.767441, dpiY = 141.402069, ret = Just 6.00   }
  , ValueTestType { dist = CssNumericPercentage 1.099854, fontSize = 11, fontXHeight = 6,  percentageBase = 11, dpiX = 141.767441, dpiY = 141.402069, ret = Just 12.0   }
  , ValueTestType { dist = CssNumericPercentage 1.099854, fontSize = 14, fontXHeight = 8,  percentageBase = 14, dpiX = 141.767441, dpiY = 141.402069, ret = Just 15.0   }
  , ValueTestType { dist = CssNumericPercentage 1.199951, fontSize = 11, fontXHeight = 6,  percentageBase = 11, dpiX = 141.767441, dpiY = 141.402069, ret = Just 13.0   }
  , ValueTestType { dist = CssNumericPercentage 1.199951, fontSize = 14, fontXHeight = 8,  percentageBase = 14, dpiX = 141.767441, dpiY = 141.402069, ret = Just 17.0   }
  , ValueTestType { dist = CssNumericPercentage 1.619873, fontSize = 14, fontXHeight = 8,  percentageBase = 14, dpiX = 141.767441, dpiY = 141.402069, ret = Just 23.0   }
  , ValueTestType { dist = CssNumericPercentage 1.799805, fontSize = 14, fontXHeight = 8,  percentageBase = 14, dpiX = 141.767441, dpiY = 141.402069, ret = Just 25.0   }
  ]




  -- On success return empty string. On failure return string representation of
-- remainder string in a row, for which test failed.
computeAbsoluteLengthValueTest :: [ValueTestType] -> Maybe T.Text
computeAbsoluteLengthValueTest []     = Nothing
computeAbsoluteLengthValueTest (x:xs) = if expected /= styleEngineComputeAbsoluteLengthValue (dist x) (fontSize x) (fontXHeight x) (percentageBase x) (dpiX x) (dpiY x)
                                        then Just . T.pack . show $ x
                                        else computeAbsoluteLengthValueTest xs
  where
    expected = ret x




testCases = [
  -- If some error is found, test function returns some data (e.g. non-empty
  -- string or test index) which can help identify which test failed.
     TestCase (do assertEqual "manual tests of css distance absolute value"            Nothing (computeAbsoluteLengthValueTest computeAbsoluteLengthValueTestData))
  ]




testsCssStyleEngine :: IO String
testsCssStyleEngine = do
  counts <- runTestTT (TestList (testCases))
  if (errors counts + failures counts == 0)
    then return ""
    else return "[EE] Hello.Tests.Css.StyleEngine failed"

