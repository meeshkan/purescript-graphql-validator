module Test.RequestValidator where

import Prelude
import Data.GraphQL.RequestValidator (validateOperationDefinitionStringAgainstSchemaAsString')
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

testRequestValidator ∷ ∀ m. Monad m ⇒ SpecT Aff Unit m Unit
testRequestValidator =
  describe "test request validator" do
    it "should work on simple query" do
      liftEffect (validateOperationDefinitionStringAgainstSchemaAsString' "{}" schema `shouldReturn` unit)
      liftEffect (validateOperationDefinitionStringAgainstSchemaAsString' "query{}" schema `shouldReturn` unit)
      liftEffect (validateOperationDefinitionStringAgainstSchemaAsString' "query { TweetsMeta { count } }" schema `shouldReturn` unit)
    it "should fail on simple query" do
      liftEffect $ expectError (validateOperationDefinitionStringAgainstSchemaAsString' "query{TweetsMeta { ount}}" schema)
    it "should work on complex query" do
      liftEffect (validateOperationDefinitionStringAgainstSchemaAsString' "query{ Tweet(id:\"88\"){id, Stats{views}, Author{first_name, tweets {id} }}}" schema `shouldReturn` unit)
    it "should fail on complex query" do
      liftEffect $ expectError (validateOperationDefinitionStringAgainstSchemaAsString' "query{ Tweet(id:\"88\"){id, Stats{views}, Author{first_nme, tweets {id} }}}" schema)
