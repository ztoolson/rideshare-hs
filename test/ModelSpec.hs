{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ModelSpec (spec) where

-- Import instances from Model

import qualified Database.Persist as DB
import Model ()
import Test.HUnit (assertFailure)
import TestImport

spec :: Spec
spec = withApp $ do
    describe "Point and Location" $ do
        it "can store and retrieve a Location with a Point" $ do
            runDB $ do
                currentTime <- liftIO getCurrentTime
                let testPoint = Point 10.5 20.7
                    testLocation =
                        Location
                            { locationAddress = "123 Test St"
                            , locationCity = "Test City"
                            , locationState = "TS"
                            , locationPosition = testPoint
                            , locationCreatedAt = currentTime
                            , locationUpdatedAt = currentTime
                            }

                locationId <- DB.insert testLocation
                maybeLocation <- DB.get locationId

                case maybeLocation of
                    Nothing -> liftIO $ assertFailure "Failed to retrieve Location"
                    Just retrievedLocation -> do
                        liftIO $ locationAddress retrievedLocation `shouldBe` locationAddress testLocation
                        liftIO $ locationCity retrievedLocation `shouldBe` locationCity testLocation
                        liftIO $ locationState retrievedLocation `shouldBe` locationState testLocation
                        liftIO $ locationPosition retrievedLocation `shouldBe` locationPosition testLocation
