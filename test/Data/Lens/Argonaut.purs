module Test.Data.Lens.Argonaut where

import Prelude

import Data.Argonaut (_Boolean, _Number, isArray, isBoolean, isNull, isNumber, isObject, isString)
import Data.Array as A
import Data.Lens as L
import Data.Lens.Fold as L
import Data.Lens.Indexed as L
import Data.Lens.Argonaut (_Json, key, nth, values, members)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Int (toNumber)
import Data.Tuple
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner (runSpec)

itoListOfOn = flip L.itoListOf

infixl 8 itoListOfOn as ^@..

argonautSpec :: Effect Unit
argonautSpec = launchAff_ $ runSpec [specReporter] do
  describe "Argonaut" do
    describe "_Json" do
      it "Should be object" do
        let o = L.preview _Json "{}"
        shouldEqual true (maybe false isObject o)
      it "Should be array" do
        let o = L.preview _Json "[]"
        shouldEqual true (maybe false isArray o)
      it "Should be null" do
        let o = L.preview _Json "null"
        shouldEqual true (maybe false isNull o)
      it "Should be booelan" do
        let o = L.preview _Json "true"
        shouldEqual true (maybe false isBoolean o)
      it "Should be string" do
        let o = L.preview _Json "\"test\""
        shouldEqual true (maybe false isString o)
      it "Should be number" do
        let o = L.preview _Json "42"
        shouldEqual true (maybe false isNumber o)
    describe "key" do
      it "should get number" do
        let o = L.preview (_Json <<< key "a" <<< _Number) """{"a":10}"""
        shouldEqual 10.0 (fromMaybe 0.0 o)
      it "should get array of numbers" do
        let o = A.fromFoldable <<< L.toListOf (_Json <<< key "a" <<< values <<< _Number) $ """{"a":[1,2,3]}"""
        shouldEqual [1.0, 2.0, 3.0] o
      it "should get array of booelans" do
        let o = A.fromFoldable <<< L.toListOf (_Json <<< key "a" <<< values <<< _Boolean) $ """{"a":[true, false, true]}"""
        shouldEqual [true, false, true, false] o
      it "should get nested value from inner object" do
        let o = """{"a":{"b":10}}""" L.^? _Json <<< key "a" <<< key "b" <<< _Number
        shouldEqual (Just 10.0) o
    describe "nth" do
      it "should get value from array" do
        let o = """"{"a": [1,2,3]}""" L.^? _Json <<< key "a" <<< nth 2 <<< _Number
        shouldEqual (Just 3.0) o
      it "should get value from array in inner arrays" do
        let o = """{"a": [[1,2,3], [4,5,6]]}""" L.^? _Json <<< key "a" <<< nth 1 <<< nth 2 <<< _Number
        shouldEqual (Just 6.0) o
      it "should get nested value fron inner array in inner object" do
        let o = """{"a":{"b":[{"x": 10}, {"x": 20}]}}""" L.^? _Json <<< key "a" <<< key "b" <<< nth 1 <<< key "x" <<< _Number
        shouldEqual (Just 20.0) o
    describe "members" do
      it "should get list of tuples" do
        let o = """{"a":4,"b":7}""" ^@.. _Json <<< members <<< _Number
        shouldEqual (List.fromFoldable [Tuple "a" 4.0, Tuple "b" 7.0]) o
      it "should get transformed value" do
        let o = """{"a":4}""" # _Json <<< (L.unIndex members) <<< _Number L.*~ 10.0
        shouldEqual """{"a":40}""" o