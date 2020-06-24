module Test.Validator where

import Prelude
import Data.GraphQL.Validator (validateJSONStringAgainstSchemaAsString')
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (expectError, shouldReturn)

schema =
  """type Tweet {
    id: ID!
    # The tweet text. No more than 140 characters!
    body: String
    # When the tweet was published
    date: Date
    # Who published the tweet
    Author: User
    # Views, retweets, likes, etc
    Stats: Stat
}

type User {
    id: ID!
    username: String
    first_name: String
    last_name: String
    full_name: String
    name: String @deprecated
    avatar_url: Url
    tweets: [Tweet!]
}

type Stat {
    views: Int
    likes: Int
    retweets: Int
    responses: Int
}

type Notification {
    id: ID
    date: Date
    type: String
}

type Meta {
    count: Int
}

scalar Url
scalar Date

type Query {
    Tweet(id: ID!): Tweet
    Tweets(limit: Int, skip: Int, sort_field: String, sort_order: String): [Tweet]
    TweetsMeta: Meta
    User(id: ID!): User
    Notifications(limit: Int): [Notification]
    NotificationsMeta: Meta
}

type Mutation {
    createTweet (
        body: String
    ): Tweet
    deleteTweet(id: ID!): Tweet
    markTweetRead(id: ID!): Boolean
}""" ∷
    String

testValidator ∷ ∀ m. Monad m ⇒ SpecT Aff Unit m Unit
testValidator =
  describe "test full spec" do
    it "should work on simple query" do
      liftEffect (validateJSONStringAgainstSchemaAsString' "{\"data\":{\"Tweet\":{\"id\":\"a\"}}}" schema `shouldReturn` unit)
      liftEffect (validateJSONStringAgainstSchemaAsString' "{\"data\":{\"Tweet\":{}}}" schema `shouldReturn` unit)
    it "should fail on simple query" do
      liftEffect $ expectError (validateJSONStringAgainstSchemaAsString' "{\"data\":{\"Tweet\":{\"id\":1}}}" schema)
    it "should work on complex query" do
      liftEffect (validateJSONStringAgainstSchemaAsString' "{\"data\":{\"Tweet\":{\"id\":\"a\",\"Author\":{\"first_name\":\"Makenna\",\"last_name\":\"Smutz\",\"tweets\":null,\"avatar_url\":\"https://github.com/KenzoBenzo/avatar\"}}}}" schema `shouldReturn` unit)
    it "should work on triple-nested query" do
      liftEffect (validateJSONStringAgainstSchemaAsString' "{\"data\":{\"Tweet\":{\"id\":\"a\",\"Author\":{\"first_name\":\"Makenna\",\"last_name\":\"Smutz\",\"tweets\":[{\"id\":\"42\"}],\"avatar_url\":\"https://github.com/KenzoBenzo/avatar\"}}}}" schema `shouldReturn` unit)
    it "should fail on complex query" do
      liftEffect $ expectError (validateJSONStringAgainstSchemaAsString' "{\"data\":{\"Tweet\":{\"id\":\"a\",\"Author\":{\"first_name\":true}}}}" schema)
