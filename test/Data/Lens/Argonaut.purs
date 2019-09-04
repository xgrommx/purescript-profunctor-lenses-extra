module Test.Data.Lens.Argonaut where

import Prelude

import Data.Argonaut (_Boolean, _Number, isArray, isBoolean, isNull, isNumber, isObject, isString)
import Data.Array as A
import Data.Lens as L
import Data.Lens.Argonaut (_Json, key, values)
import Data.Maybe (fromMaybe, maybe)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner (runSpec)

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