{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}




module Hello.Tests.Css.Match.Data
  (
    matchTestManualData
  )
where




import qualified Data.Text as T

import Hello.Css.Parser
import Css




-- Tests of calculating matching of Css selector.
--
-- This array is called "Manual" because these tests were entered manually.
-- They come from comparing result of old C++ function and new Haskell
-- function, and printing the input selector in Haskell function.
--
-- Perhaps in the future I will write some generator of test data.
matchTestManualData :: [(Int, CssCompoundSelector, DoctreeNode)] = [
    (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 36) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 453, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 655, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 61) {- c= -}  [] {- pc = -}["first-child"] {- id= -} "",
    DoctreeNode {uniqueNum = 181, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 729, htmlElementIdx = 0, selPseudoClass = "visited", selId = "", selClass = ["STC"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "links",
    DoctreeNode {uniqueNum = 660, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 560, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 36) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 249, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 57, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["submemu"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1066, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 236, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1146, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["hover"] {- id= -} "",
    DoctreeNode {uniqueNum = 992, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["data_head"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 988, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["intro"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1005, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 456, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["commentBody"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1112, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 61) {- c= -}  [] {- pc = -}["first-child"] {- id= -} "",
    DoctreeNode {uniqueNum = 1052, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "links",
    DoctreeNode {uniqueNum = 0, htmlElementIdx = 42, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 218, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "olderstuff-content",
    DoctreeNode {uniqueNum = 567, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 720, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["generaltitle"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 822, htmlElementIdx = 7, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 360, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 883, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 232, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 278, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 466, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 143, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1089, htmlElementIdx = 73, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 646, htmlElementIdx = 46, selPseudoClass = "", selId = "more_21/09/09/0126238", selClass = ["story_more"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 283, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1161, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 755, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 590, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  ["menu"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 95, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1016, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 35) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 142, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["whysub"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 740, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 988, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["intro"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 479, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["intro"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "olderstuff-content",
    DoctreeNode {uniqueNum = 28, htmlElementIdx = 21, selPseudoClass = "", selId = "frame", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["generalbody"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 526, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["generaltitle"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "formtabs",
    DoctreeNode {uniqueNum = 958, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["storylinks"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 143, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 444, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 843, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 656, htmlElementIdx = 69, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 567, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["details"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 867, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 531, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 174, htmlElementIdx = 12, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 783, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 162, htmlElementIdx = 12, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  ["details"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1028, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["details"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 562, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["more"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 783, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["article"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 397, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["generaltitle"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1019, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1138, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "section_banner",
    DoctreeNode {uniqueNum = 714, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["comments"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 71, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 1166, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 218, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 972, htmlElementIdx = 35, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["selected"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 135, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 729, htmlElementIdx = 0, selPseudoClass = "visited", selId = "", selClass = ["STC"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 36) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 465, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["details"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 942, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 331, htmlElementIdx = 36, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 114, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 508, htmlElementIdx = 41, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 62, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 936, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["btmnav"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1137, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["btmnav"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 424, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 708, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["storylinks"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 511, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1131, htmlElementIdx = 24, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 74, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "links",
    DoctreeNode {uniqueNum = 1161, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 714, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["comments"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 271, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 366, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 986, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["hover"] {- id= -} "",
    DoctreeNode {uniqueNum = 477, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 28, htmlElementIdx = 21, selPseudoClass = "", selId = "frame", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 449, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  ["menu"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 89, htmlElementIdx = 21, selPseudoClass = "", selId = "sections-content", selClass = ["content"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1106, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["article"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 612, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 328, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "links",
    DoctreeNode {uniqueNum = 189, htmlElementIdx = 21, selPseudoClass = "", selId = "poll-block", selClass = ["block"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["commentTop"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 865, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["storylinks"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["hover"] {- id= -} "",
    DoctreeNode {uniqueNum = 749, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 92, htmlElementIdx = 0, selPseudoClass = "visited", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 419, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 240, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 327, htmlElementIdx = 21, selPseudoClass = "", selId = "nextPrev", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 987, htmlElementIdx = 45, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 138, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 306, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 537, htmlElementIdx = 12, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 71) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 613, htmlElementIdx = 71, selPseudoClass = "", selId = "", selClass = ["sd-info-block"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 367, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 558, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 203, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 881, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["details"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 367, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 869, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["hover"] {- id= -} "",
    DoctreeNode {uniqueNum = 1005, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["commentTop"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1139, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["begin"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 120, htmlElementIdx = 0, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1046, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}["first-child", "before"] {- id= -} "",
    DoctreeNode {uniqueNum = 268, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 24) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 276, htmlElementIdx = 24, selPseudoClass = "", selId = "", selClass = ["comment_count"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 703, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1101, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 717, htmlElementIdx = 71, selPseudoClass = "", selId = "", selClass = ["sd-info-block"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["hover"] {- id= -} "",
    DoctreeNode {uniqueNum = 360, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 361, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 917, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 878, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["title"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 351, htmlElementIdx = 12, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "slashboxes",
    DoctreeNode {uniqueNum = 121, htmlElementIdx = 21, selPseudoClass = "", selId = "slashboxes", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 482, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1129, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 90, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 629, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["topic"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 758, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  ["details"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 401, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["details"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["commentBody"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 796, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["intro"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["link"] {- id= -} "",
    DoctreeNode {uniqueNum = 1019, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 250, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["hover"] {- id= -} "",
    DoctreeNode {uniqueNum = 367, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 656, htmlElementIdx = 69, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 197, htmlElementIdx = 46, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 664, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 860, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 707, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 61) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 955, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 382, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 961, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["more"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 67, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 577, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1038, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["body"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 411, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["body"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 191, htmlElementIdx = 36, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1157, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 406, htmlElementIdx = 0, selPseudoClass = "visited", selId = "", selClass = ["STC"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 504, htmlElementIdx = 24, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 978, htmlElementIdx = 74, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["comments"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 189, htmlElementIdx = 21, selPseudoClass = "", selId = "poll-block", selClass = ["block"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 56, htmlElementIdx = 21, selPseudoClass = "", selId = "navigation-content", selClass = ["content"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 680, htmlElementIdx = 74, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1024, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["generaltitle"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 96, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "links",
    DoctreeNode {uniqueNum = 90, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 61) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 444, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 946, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 868, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["more"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["hover"] {- id= -} "",
    DoctreeNode {uniqueNum = 869, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 683, htmlElementIdx = 12, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 263, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 483, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "slashboxes",
    DoctreeNode {uniqueNum = 53, htmlElementIdx = 21, selPseudoClass = "", selId = "navigation-block", selClass = ["block"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["briefarticle"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 541, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["topic"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["title"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 776, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 23, htmlElementIdx = 0, selPseudoClass = "visited", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 35) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 245, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 505, htmlElementIdx = 7, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 193, htmlElementIdx = 21, selPseudoClass = "", selId = "poll-content", selClass = ["content"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 7) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 846, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 436, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 120, htmlElementIdx = 0, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 190, htmlElementIdx = 21, selPseudoClass = "", selId = "poll-title", selClass = ["title"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 429, htmlElementIdx = 46, selPseudoClass = "", selId = "more_21/09/09/204232", selClass = ["story_more"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 574, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 284, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 749, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 35) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 635, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 218, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1037, htmlElementIdx = 7, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 470, htmlElementIdx = 0, selPseudoClass = "visited", selId = "", selClass = ["STC"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 593, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 501, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 838, htmlElementIdx = 0, selPseudoClass = "visited", selId = "", selClass = ["STC"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  ["storylinks"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 958, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["storylinks"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 452, htmlElementIdx = 7, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1052, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 959, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["hover"] {- id= -} "",
    DoctreeNode {uniqueNum = 269, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1008, htmlElementIdx = 41, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 269, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 10) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 901, htmlElementIdx = 10, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 759, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 319, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 257, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["selected"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1137, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["btmnav"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 957, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 43, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1056, htmlElementIdx = 7, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 50, htmlElementIdx = 34, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 36) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 660, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 20, htmlElementIdx = 0, selPseudoClass = "visited", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 893, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["commentBox"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 943, htmlElementIdx = 10, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1046, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 1152, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1138, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 281, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 480, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = ["byline"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 794, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 233, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 336, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["begin"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 264, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "links",
    DoctreeNode {uniqueNum = 970, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["generaltitle"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["details"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 661, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["more"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 171, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 34) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 416, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = ["byline"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 203, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "section_banner",
    DoctreeNode {uniqueNum = 398, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["title"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 52, htmlElementIdx = 21, selPseudoClass = "", selId = "links", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 222, htmlElementIdx = 7, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 955, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 726, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 910, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["storylinks"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 99, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 252, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["link"] {- id= -} "",
    DoctreeNode {uniqueNum = 558, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["link"] {- id= -} "",
    DoctreeNode {uniqueNum = 685, htmlElementIdx = 7, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1040, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 540, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["body"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 370, htmlElementIdx = 46, selPseudoClass = "", selId = "more_21/09/10/0426236", selClass = ["story_more"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["hover"] {- id= -} "",
    DoctreeNode {uniqueNum = 1127, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 763, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 193, htmlElementIdx = 21, selPseudoClass = "", selId = "poll-content", selClass = ["content"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 216, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["selected"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 668, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}["before"] {- id= -} "",
    DoctreeNode {uniqueNum = 268, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 789, htmlElementIdx = 0, selPseudoClass = "visited", selId = "", selClass = ["STC"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["intro"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1048, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 94, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 801, htmlElementIdx = 10, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 592, htmlElementIdx = 10, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 530, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["details"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 311, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["selected"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1028, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["details"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1148, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["hover"] {- id= -} "",
    DoctreeNode {uniqueNum = 860, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["selected"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 340, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["generaltitle"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 326, htmlElementIdx = 7, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 382, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 90, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 36) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1123, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["more"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 36) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 361, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 369, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["comments"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 317, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 140, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["logout"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 964, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["comments"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1067, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "links",
    DoctreeNode {uniqueNum = 918, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 585, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1148, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 487, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 7) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 693, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 6, htmlElementIdx = 51, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 113, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 340, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["generaltitle"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 335, htmlElementIdx = 21, selPseudoClass = "", selId = "articles", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 81, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 305, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["details"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 455, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["comments"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["hover"] {- id= -} "",
    DoctreeNode {uniqueNum = 869, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 584, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["topic"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 373, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1157, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 519, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1152, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 147, htmlElementIdx = 21, selPseudoClass = "", selId = "site_news-content", selClass = ["content"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 343, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["commentBox"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 19, htmlElementIdx = 11, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 10) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 938, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["intro"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 64, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 556, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["begin"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 475, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["body"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 61) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 148, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "olderstuff-content",
    DoctreeNode {uniqueNum = 448, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 328, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 33, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 415, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["intro"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1060, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 589, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 622, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 281, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 953, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1003, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 113, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 959, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 61) {- c= -}  [] {- pc = -}["last-child"] {- id= -} "",
    DoctreeNode {uniqueNum = 754, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 268, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["details"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 181, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 75, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 109, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1139, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["begin"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 509, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 36) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 926, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["details"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 271, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 558, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 36) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 274, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 436, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 46) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 491, htmlElementIdx = 46, selPseudoClass = "", selId = "more_21/09/09/1958231", selClass = ["story_more"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 43, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1027, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 61) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 999, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 964, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["comments"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 815, htmlElementIdx = 69, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 535, htmlElementIdx = 0, selPseudoClass = "visited", selId = "", selClass = ["STC"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 887, htmlElementIdx = 73, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 1129, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 668, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 137, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["article"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 522, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["briefarticle"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1018, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["comments"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 36) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1095, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["intro"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 195, htmlElementIdx = 30, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1107, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  [] {- pc = -}[] {- id= -} "frame",
    DoctreeNode {uniqueNum = 28, htmlElementIdx = 21, selPseudoClass = "", selId = "frame", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 737, htmlElementIdx = 45, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 768, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["storylinks"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 435, htmlElementIdx = 24, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 353, htmlElementIdx = 7, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 445, htmlElementIdx = 69, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 480, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = ["byline"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 622, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 865, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["storylinks"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["hover"] {- id= -} "",
    DoctreeNode {uniqueNum = 566, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 678, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["purchasesubscription__duration"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 227, htmlElementIdx = 21, selPseudoClass = "", selId = "top10journals-content", selClass = ["content"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 440, htmlElementIdx = 7, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 446, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 182, htmlElementIdx = 7, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1122, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 33) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 938, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["intro"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["selected"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1024, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["generaltitle"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 720, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["generaltitle"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "links",
    DoctreeNode {uniqueNum = 522, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["details"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 142, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["whysub"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 402, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 35, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 326, htmlElementIdx = 7, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 757, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["link"] {- id= -} "",
    DoctreeNode {uniqueNum = 73, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "section_banner",
    DoctreeNode {uniqueNum = 594, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 944, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["selected"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 996, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 771, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["more"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 840, htmlElementIdx = 12, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["details"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 908, htmlElementIdx = 69, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 218, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 145, htmlElementIdx = 21, selPseudoClass = "", selId = "site_news-title", selClass = ["title"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 438, htmlElementIdx = 12, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 507, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["link"] {- id= -} "",
    DoctreeNode {uniqueNum = 704, htmlElementIdx = 41, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1057, htmlElementIdx = 12, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 61) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 653, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  [] {- pc = -}[] {- id= -} "frame",
    DoctreeNode {uniqueNum = 28, htmlElementIdx = 21, selPseudoClass = "", selId = "frame", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "slashboxes",
    DoctreeNode {uniqueNum = 121, htmlElementIdx = 21, selPseudoClass = "", selId = "slashboxes", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 535, htmlElementIdx = 0, selPseudoClass = "visited", selId = "", selClass = ["STC"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1080, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 258, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  ["STC"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 886, htmlElementIdx = 0, selPseudoClass = "visited", selId = "", selClass = ["STC"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 61) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 951, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 107, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 77, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 1144, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 755, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["details"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1108, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 476, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["topic"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 10) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 994, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["link"] {- id= -} "",
    DoctreeNode {uniqueNum = 106, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 449, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 328, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 561, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 36) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 286, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 128, htmlElementIdx = 46, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 457, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 11) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 19, htmlElementIdx = 11, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 61) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 747, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["generalbody"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 720, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["generaltitle"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1020, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "topnav",
    DoctreeNode {uniqueNum = 264, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1085, htmlElementIdx = 74, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 98, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 225, htmlElementIdx = 36, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 649, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 467, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 35, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 518, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 875, htmlElementIdx = 71, selPseudoClass = "", selId = "", selClass = ["sd-key-sid"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 936, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 308, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 36) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 573, htmlElementIdx = 35, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["submenu"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 448, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 119, htmlElementIdx = 21, selPseudoClass = "", selId = "index", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 50, htmlElementIdx = 34, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 267, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["selected"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 450, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["more"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["briefarticle"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 182, htmlElementIdx = 7, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 838, htmlElementIdx = 0, selPseudoClass = "visited", selId = "", selClass = ["STC"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 270, htmlElementIdx = 24, selPseudoClass = "", selId = "", selClass = ["comment_count"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 218, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 151, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "login_box",
    DoctreeNode {uniqueNum = 567, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 563, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1063, htmlElementIdx = 69, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 396, htmlElementIdx = 71, selPseudoClass = "", selId = "", selClass = ["type"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 498, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["intro"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 696, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 352, htmlElementIdx = 73, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 805, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 7) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 896, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = ["byline"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 61) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 642, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["link"] {- id= -} "",
    DoctreeNode {uniqueNum = 1077, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["generaltitle"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -} [] {- pc = -}["first-child", "before"] {- id= -} "",
    DoctreeNode {uniqueNum = 245, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 104, htmlElementIdx = 21, selPseudoClass = "", selId = "sitebox-content", selClass = ["content"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 193, htmlElementIdx = 21, selPseudoClass = "", selId = "poll-content", selClass = ["content"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 73, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 801, htmlElementIdx = 10, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 914, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 171, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["hover"] {- id= -} "",
    DoctreeNode {uniqueNum = 1016, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 10) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 776, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["selected"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 769, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 163, htmlElementIdx = 7, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "section_banner",
    DoctreeNode {uniqueNum = 612, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 819, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["hover"] {- id= -} "",
    DoctreeNode {uniqueNum = 880, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 467, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1137, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["btmnav"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 94, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 1119, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1097, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 767, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 789, htmlElementIdx = 0, selPseudoClass = "visited", selId = "", selClass = ["STC"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 908, htmlElementIdx = 69, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 601, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 1115, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 514, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 519, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 116, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 10) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 901, htmlElementIdx = 10, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 841, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["body"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 897, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1098, htmlElementIdx = 10, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 268, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  [] {- pc = -}[] {- id= -} "frame",
    DoctreeNode {uniqueNum = 28, htmlElementIdx = 21, selPseudoClass = "", selId = "frame", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 1142, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "slashboxes",
    DoctreeNode {uniqueNum = 612, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 927, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["article"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 792, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["body"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["intro"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 796, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["intro"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 36) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 110, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 916, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["comments"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 240, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 114, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1148, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 232, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 849, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["data_head"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 463, htmlElementIdx = 35, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 812, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 184, htmlElementIdx = 12, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 532, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 762, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 227, htmlElementIdx = 21, selPseudoClass = "", selId = "top10journals-content", selClass = ["content"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["quote"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 984, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["body"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 634, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 412, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["topic"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 230, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 716, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1153, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 541, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["topic"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 302, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 561, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 53, htmlElementIdx = 21, selPseudoClass = "", selId = "navigation-block", selClass = ["block"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 667, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  ["menu"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 90, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 812, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 145, htmlElementIdx = 21, selPseudoClass = "", selId = "site_news-title", selClass = ["title"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 61) {- c= -}  [] {- pc = -}["first-child"] {- id= -} "",
    DoctreeNode {uniqueNum = 754, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["data_head"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 399, htmlElementIdx = 35, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 385, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 942, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 36) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 974, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["details"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 306, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 563, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 514, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 226, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 42) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 0, htmlElementIdx = 42, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 483, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 278, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 758, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 303, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 865, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["storylinks"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1078, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["title"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1020, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 386, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["more"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["hover"] {- id= -} "",
    DoctreeNode {uniqueNum = 723, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 151, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 445, htmlElementIdx = 69, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1102, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 185, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 721, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["title"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 35) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 342, htmlElementIdx = 35, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 214, htmlElementIdx = 46, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 134, htmlElementIdx = 41, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 398, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["title"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  [] {- pc = -}[] {- id= -} "articles",
    DoctreeNode {uniqueNum = 335, htmlElementIdx = 21, selPseudoClass = "", selId = "articles", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 549, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 583, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["body"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "links",
    DoctreeNode {uniqueNum = 1155, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["selected"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 984, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["body"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  ["article"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 918, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 70, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 61) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1050, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["hover"] {- id= -} "",
    DoctreeNode {uniqueNum = 998, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1049, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  ["storylinks"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1130, htmlElementIdx = 21, selPseudoClass = "", selId = "footer", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1140, htmlElementIdx = 0, selPseudoClass = "visited", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["commentBox"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1039, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["topic"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "polls-wide",
    DoctreeNode {uniqueNum = 448, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1139, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["begin"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["link"] {- id= -} "",
    DoctreeNode {uniqueNum = 872, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1162, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 36) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 705, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 94, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 996, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  ["menu"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 575, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["details"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 472, htmlElementIdx = 12, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 109, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 835, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 665, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["begin"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1020, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "links",
    DoctreeNode {uniqueNum = 90, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 19, htmlElementIdx = 11, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 601, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 292, htmlElementIdx = 24, selPseudoClass = "", selId = "", selClass = ["comment_count"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 36) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 908, htmlElementIdx = 69, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 528, htmlElementIdx = 35, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 36) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 961, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["more"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["intro"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 421, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 882, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["selected"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 247, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 60, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 939, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = ["byline"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 862, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 28, htmlElementIdx = 21, selPseudoClass = "", selId = "frame", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  ["commtree"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1014, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 80, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 61) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1096, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = ["byline"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1136, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["copyright"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["hover"] {- id= -} "",
    DoctreeNode {uniqueNum = 107, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 979, htmlElementIdx = 0, selPseudoClass = "visited", selId = "", selClass = ["STC"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["data_head"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1143, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 0, htmlElementIdx = 42, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["selected"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 686, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["body"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 970, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["generaltitle"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["comments"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1159, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 691, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = ["byline"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 35) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 926, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["details"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 380, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["link"] {- id= -} "",
    DoctreeNode {uniqueNum = 1137, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["btmnav"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 82, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["hover"] {- id= -} "",
    DoctreeNode {uniqueNum = 667, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["hover"] {- id= -} "",
    DoctreeNode {uniqueNum = 965, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 71, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["comments"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 559, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["storylinks"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 918, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 256, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["generaltitle"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 829, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["generaltitle"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 916, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["comments"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 83, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 172, htmlElementIdx = 71, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["commentBox"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1155, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 143, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 435, htmlElementIdx = 24, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 61) {- c= -}  [] {- pc = -}["first-child"] {- id= -} "",
    DoctreeNode {uniqueNum = 427, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 764, htmlElementIdx = 41, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 833, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["details"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 191, htmlElementIdx = 36, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 35) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1149, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  ["STC"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 729, htmlElementIdx = 0, selPseudoClass = "visited", selId = "", selClass = ["STC"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 788, htmlElementIdx = 74, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 281, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 769, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 524, htmlElementIdx = 71, selPseudoClass = "", selId = "", selClass = ["sd-key-sid"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 526, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["generaltitle"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 35) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 588, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = ["byline"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 164, htmlElementIdx = 12, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "commentlisting",
    DoctreeNode {uniqueNum = 101, htmlElementIdx = 21, selPseudoClass = "", selId = "sitebox-block", selClass = ["block"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["link"] {- id= -} "",
    DoctreeNode {uniqueNum = 1074, htmlElementIdx = 71, selPseudoClass = "", selId = "", selClass = ["sd-info-block"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  ["details"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 784, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["details"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["begin"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 676, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["details"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 662, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 606, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 805, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1020, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 260, htmlElementIdx = 21, selPseudoClass = "", selId = "olderstuff-title", selClass = ["title"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "slashboxes",
    DoctreeNode {uniqueNum = 190, htmlElementIdx = 21, selPseudoClass = "", selId = "poll-title", selClass = ["title"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1151, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["begin"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1117, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 798, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["title"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 398, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["title"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 107, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["article"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 781, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["title"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["commentTop"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 972, htmlElementIdx = 35, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  ["article"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1073, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["hover"] {- id= -} "",
    DoctreeNode {uniqueNum = 67, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 715, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["data_head"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1050, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1070, htmlElementIdx = 7, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 824, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 622, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  ["article"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 457, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 341, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["title"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 25, htmlElementIdx = 0, selPseudoClass = "visited", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 562, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["more"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["purchasesubscription__duration"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 52, htmlElementIdx = 21, selPseudoClass = "", selId = "links", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  ["storylinks"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 708, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["storylinks"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 442, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1097, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 157, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 964, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["comments"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 61) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1062, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["hover"] {- id= -} "",
    DoctreeNode {uniqueNum = 85, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 104, htmlElementIdx = 21, selPseudoClass = "", selId = "sitebox-content", selClass = ["content"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 653, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["briefarticle"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 70, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 799, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 701, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 962, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 1011, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 964, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["comments"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["briefarticle"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1091, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["body"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 84, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 513, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 344, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["details"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 43, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["link"] {- id= -} "",
    DoctreeNode {uniqueNum = 914, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["link"] {- id= -} "",
    DoctreeNode {uniqueNum = 494, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 324, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["article"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 845, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["intro"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 749, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 428, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["selected"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 241, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 96, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 125, htmlElementIdx = 21, selPseudoClass = "", selId = "userlogin-content", selClass = ["content"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 930, htmlElementIdx = 74, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "links",
    DoctreeNode {uniqueNum = 716, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 34) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 633, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = ["byline"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 416, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = ["byline"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 61) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 746, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 220, htmlElementIdx = 12, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 28, htmlElementIdx = 21, selPseudoClass = "", selId = "frame", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 794, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1132, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["search"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1082, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["selected"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 664, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 379, htmlElementIdx = 41, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["details"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 752, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 736, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["selected"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 259, htmlElementIdx = 21, selPseudoClass = "", selId = "olderstuff-block", selClass = ["block"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["details"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 881, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["details"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["briefarticle"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 101, htmlElementIdx = 21, selPseudoClass = "", selId = "sitebox-block", selClass = ["block"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 813, htmlElementIdx = 41, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1050, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 895, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["intro"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 864, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 403, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 747, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 899, htmlElementIdx = 43, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 35) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 618, htmlElementIdx = 35, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 234, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 255, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 89, htmlElementIdx = 21, selPseudoClass = "", selId = "sections-content", selClass = ["content"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 804, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 154, htmlElementIdx = 41, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 216, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 961, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["more"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1018, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["comments"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1122, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 330, htmlElementIdx = 21, selPseudoClass = "", selId = "index_qlinks-title", selClass = ["title"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1047, htmlElementIdx = 10, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 69, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 574, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 36) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 802, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["commentBox"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 31, htmlElementIdx = 33, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 7) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 594, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 805, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 874, htmlElementIdx = 71, selPseudoClass = "", selId = "", selClass = ["sd-info-block"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["selected"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 501, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 660, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 382, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 61) {- c= -}  [] {- pc = -}["last-child"] {- id= -} "",
    DoctreeNode {uniqueNum = 553, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["link"] {- id= -} "",
    DoctreeNode {uniqueNum = 808, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 10) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 944, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["selected"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 878, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["title"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 438, htmlElementIdx = 12, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 677, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 242, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 637, htmlElementIdx = 10, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  ["storylinks"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 865, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["storylinks"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 590, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 58) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 709, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 619, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 403, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 269, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 137, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 775, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  ["details"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 833, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["details"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["link"] {- id= -} "",
    DoctreeNode {uniqueNum = 919, htmlElementIdx = 71, selPseudoClass = "", selId = "", selClass = ["sd-info-block"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 761, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 36) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 74, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "section_banner",
    DoctreeNode {uniqueNum = 194, htmlElementIdx = 21, selPseudoClass = "", selId = "pollbody", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["selected"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1043, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = ["byline"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 585, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 643, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 664, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 36) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1104, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 42) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 0, htmlElementIdx = 42, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "editComment",
    DoctreeNode {uniqueNum = 686, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["body"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 430, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["story_more","full"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 169, htmlElementIdx = 71, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 100, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1045, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["selected"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 716, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 971, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["title"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 119, htmlElementIdx = 21, selPseudoClass = "", selId = "index", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 195, htmlElementIdx = 30, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 319, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 292, htmlElementIdx = 24, selPseudoClass = "", selId = "", selClass = ["comment_count"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 160, htmlElementIdx = 12, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["selected"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 557, htmlElementIdx = 69, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 440, htmlElementIdx = 7, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 560, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["hover"] {- id= -} "",
    DoctreeNode {uniqueNum = 546, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 7) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 223, htmlElementIdx = 21, selPseudoClass = "", selId = "top10journals", selClass = ["block"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 990, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 889, htmlElementIdx = 73, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "formtabs",
    DoctreeNode {uniqueNum = 1065, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["storylinks"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 41, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["selected"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 673, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["title"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["selected"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 820, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["more"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["quote"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 372, htmlElementIdx = 10, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 40, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 216, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 425, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1149, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 138, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 413, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["selected"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 955, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 986, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["link"] {- id= -} "",
    DoctreeNode {uniqueNum = 567, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["hover"] {- id= -} "",
    DoctreeNode {uniqueNum = 92, htmlElementIdx = 0, selPseudoClass = "visited", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 35) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 336, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["commentTop"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 335, htmlElementIdx = 21, selPseudoClass = "", selId = "articles", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 121, htmlElementIdx = 21, selPseudoClass = "", selId = "slashboxes", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  ["commtree"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 228, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1103, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 776, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 53, htmlElementIdx = 21, selPseudoClass = "", selId = "navigation-block", selClass = ["block"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1122, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1152, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "links",
    DoctreeNode {uniqueNum = 1141, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 647, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["story_more","full"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 744, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1158, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 301, htmlElementIdx = 24, selPseudoClass = "", selId = "", selClass = ["comment_count"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 114, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 727, htmlElementIdx = 45, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 58) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1138, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 513, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1152, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 67, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 61) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1102, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 630, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 242, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 630, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 316, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["purchasesubscription__duration"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 513, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["generalbody"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 457, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1122, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 34) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 891, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["body"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 153, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 118, htmlElementIdx = 21, selPseudoClass = "", selId = "contents", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 849, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 931, htmlElementIdx = 0, selPseudoClass = "visited", selId = "", selClass = ["STC"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1160, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 260, htmlElementIdx = 21, selPseudoClass = "", selId = "olderstuff-title", selClass = ["title"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1077, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["generaltitle"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "links",
    DoctreeNode {uniqueNum = 271, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 574, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["commentBox"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 476, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["topic"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 304, htmlElementIdx = 24, selPseudoClass = "", selId = "", selClass = ["comment_count"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 834, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 218, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 545, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 736, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["link"] {- id= -} "",
    DoctreeNode {uniqueNum = 86, htmlElementIdx = 21, selPseudoClass = "", selId = "sections-block", selClass = ["block"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  [] {- pc = -}[] {- id= -} "contents",
    DoctreeNode {uniqueNum = 118, htmlElementIdx = 21, selPseudoClass = "", selId = "contents", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 137, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 609, htmlElementIdx = 7, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1044, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  [] {- pc = -}[] {- id= -} "contents",
    DoctreeNode {uniqueNum = 118, htmlElementIdx = 21, selPseudoClass = "", selId = "contents", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1153, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 61) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1050, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 618, htmlElementIdx = 35, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 362, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 711, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["more"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 664, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 625, htmlElementIdx = 0, selPseudoClass = "visited", selId = "", selClass = ["STC"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 997, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 660, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  [] {- pc = -}[] {- id= -} "articles",
    DoctreeNode {uniqueNum = 335, htmlElementIdx = 21, selPseudoClass = "", selId = "articles", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 937, htmlElementIdx = 45, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 210, htmlElementIdx = 46, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 236, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1106, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["selected"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1010, htmlElementIdx = 69, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 818, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 908, htmlElementIdx = 69, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 33) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 868, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["more"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 389, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 96, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 64, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 58, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 35) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 573, htmlElementIdx = 35, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 336, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["selected"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 706, htmlElementIdx = 69, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1142, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 800, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 36) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 191, htmlElementIdx = 36, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 33) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 463, htmlElementIdx = 35, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 61) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 765, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["link"] {- id= -} "",
    DoctreeNode {uniqueNum = 781, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["title"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 803, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["hover"] {- id= -} "",
    DoctreeNode {uniqueNum = 288, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 455, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["comments"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["link"] {- id= -} "",
    DoctreeNode {uniqueNum = 147, htmlElementIdx = 21, selPseudoClass = "", selId = "site_news-content", selClass = ["content"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "links",
    DoctreeNode {uniqueNum = 56, htmlElementIdx = 21, selPseudoClass = "", selId = "navigation-content", selClass = ["content"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 28, htmlElementIdx = 21, selPseudoClass = "", selId = "frame", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  [] {- pc = -}[] {- id= -} "frame",
    DoctreeNode {uniqueNum = 28, htmlElementIdx = 21, selPseudoClass = "", selId = "frame", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 35) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 734, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["body"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 783, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 436, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 864, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 33) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 62, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["link"] {- id= -} "",
    DoctreeNode {uniqueNum = 43, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 153, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 621, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 760, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "links",
    DoctreeNode {uniqueNum = 868, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["more"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "section_banner",
    DoctreeNode {uniqueNum = 33, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 391, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["comments"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 42) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 0, htmlElementIdx = 42, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["article"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 457, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 10) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 363, htmlElementIdx = 10, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  ["menu"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 911, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 479, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["intro"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 966, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 729, htmlElementIdx = 0, selPseudoClass = "visited", selId = "", selClass = ["STC"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 333, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["hover"] {- id= -} "",
    DoctreeNode {uniqueNum = 1064, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["hover"] {- id= -} "",
    DoctreeNode {uniqueNum = 67, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  ["storylinks"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 910, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["storylinks"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["briefarticle"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 819, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 588, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = ["byline"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 36) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 918, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 195, htmlElementIdx = 30, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 61) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 752, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 71) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 826, htmlElementIdx = 71, selPseudoClass = "", selId = "", selClass = ["sd-info-block"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 33) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 796, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["intro"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["selected"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 541, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["topic"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 918, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 243, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 900, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 402, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 946, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 139, htmlElementIdx = 41, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 403, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 11) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 19, htmlElementIdx = 11, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 476, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["topic"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 36) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 955, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "links",
    DoctreeNode {uniqueNum = 1026, htmlElementIdx = 35, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 80, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 847, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 634, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 480, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = ["byline"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1040, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 681, htmlElementIdx = 0, selPseudoClass = "visited", selId = "", selClass = ["STC"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 533, htmlElementIdx = 45, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "links",
    DoctreeNode {uniqueNum = 28, htmlElementIdx = 21, selPseudoClass = "", selId = "frame", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 797, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = ["byline"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 257, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 861, htmlElementIdx = 41, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["details"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 482, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1092, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["topic"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["selected"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 482, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1083, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["btmnav"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 262, htmlElementIdx = 21, selPseudoClass = "", selId = "olderstuff-content", selClass = ["content"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 1069, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 923, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["title"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 323, htmlElementIdx = 7, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 611, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "section_banner",
    DoctreeNode {uniqueNum = 1071, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["comments"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["generaltitle"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 923, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["title"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  ["article"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 716, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 477, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1080, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 65, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 767, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["link"] {- id= -} "",
    DoctreeNode {uniqueNum = 1073, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1148, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  ["menu"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 960, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  ["menu"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 628, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["body"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["hover"] {- id= -} "",
    DoctreeNode {uniqueNum = 79, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1064, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1148, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["commentTop"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 359, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = ["byline"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 252, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  ["STC"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 580, htmlElementIdx = 0, selPseudoClass = "visited", selId = "", selClass = ["STC"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  [] {- pc = -}[] {- id= -} "index",
    DoctreeNode {uniqueNum = 119, htmlElementIdx = 21, selPseudoClass = "", selId = "index", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["article"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 966, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  ["details"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 974, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["details"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 519, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 244, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 677, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 511, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 990, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 1067, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["details"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 866, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["generaltitle"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 53, htmlElementIdx = 21, selPseudoClass = "", selId = "navigation-block", selClass = ["block"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "commentlisting",
    DoctreeNode {uniqueNum = 659, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "links",
    DoctreeNode {uniqueNum = 1025, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["title"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 36) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 955, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 7) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1151, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["details"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 90, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 546, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1115, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 276, htmlElementIdx = 24, selPseudoClass = "", selId = "", selClass = ["comment_count"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1055, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 936, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 470, htmlElementIdx = 0, selPseudoClass = "visited", selId = "", selClass = ["STC"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["generaltitle"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 527, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["title"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 635, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 453, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["article"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 877, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["generaltitle"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1123, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = ["more"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 61) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 703, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 430, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["story_more","full"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["hover"] {- id= -} "",
    DoctreeNode {uniqueNum = 100, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 7) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 228, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["link"] {- id= -} "",
    DoctreeNode {uniqueNum = 168, htmlElementIdx = 12, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 35) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1143, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["details"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1079, htmlElementIdx = 35, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 642, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 467, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1124, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 571, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["generaltitle"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 247, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 61) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1049, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 818, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 603, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["comments"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 46, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["data_head"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1155, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  ["selected"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1157, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 296, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 63, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1157, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 966, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1122, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 427, htmlElementIdx = 61, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 755, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 85) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 1067, htmlElementIdx = 85, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 1082, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["menuoldstyle"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 559, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["storylinks"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 240, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 830, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["title"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 470, htmlElementIdx = 0, selPseudoClass = "visited", selId = "", selClass = ["STC"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 21) {- c= -}  ["storylinks"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 512, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["storylinks"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["hover"] {- id= -} "",
    DoctreeNode {uniqueNum = 1106, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 321, htmlElementIdx = 12, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (3,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 0) {- c= -}  [] {- pc = -}["visited"] {- id= -} "",
    DoctreeNode {uniqueNum = 753, htmlElementIdx = 0, selPseudoClass = "link", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (1,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  ["commentTop"] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 716, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["article"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 861, htmlElementIdx = 41, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 251, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "section_banner",
    DoctreeNode {uniqueNum = 99, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 36) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 401, htmlElementIdx = 21, selPseudoClass = "", selId = "", selClass = ["details"], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (0,
    mkCssCompoundSelector {- t= -} (CssTypeSelector 50) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 72, htmlElementIdx = 50, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (4,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUnknown) {- c= -}  [] {- pc = -}[] {- id= -} "",
    DoctreeNode {uniqueNum = 706, htmlElementIdx = 69, selPseudoClass = "", selId = "", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  , (2,
    mkCssCompoundSelector {- t= -} (CssTypeSelectorUniv) {- c= -}  [] {- pc = -}[] {- id= -} "links",
    DoctreeNode {uniqueNum = 29, htmlElementIdx = 21, selPseudoClass = "", selId = "topnav", selClass = [], parent = Nothing, sibling = Nothing, lastChild = Nothing})
  ]


