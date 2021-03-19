{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Telegram.Bot.API.InlineMode where

import Data.Aeson
import Data.Coerce (coerce)
import Data.Hashable (Hashable)
import Data.Int (Int32, Int64)
import Data.Proxy
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)
import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

newtype InlineQueryId = InlineQueryId Text
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

-- | This object represents an incoming inline query.
-- When the user sends an empty query, your bot could return some default or trending results.
data InlineQuery
  = InlineQuery
      { -- | Unique identifier for this query.
        inlineQueryId :: InlineQueryId,
        -- | Sender.
        inlineQueryFrom :: User,
        -- | Sender location, only for bots that request user location.
        inlineQueryLocation :: Maybe Location,
        -- | Text of the query (up to 256 characters).
        inlineQueryQuery :: Text,
        -- | Offset of the results to be returned, can be controlled by the bot.
        inlineQueryOffset :: Text
      }
  deriving (Generic, Show)

newtype ChosenInlineResultId = ChosenInlineResultId Int32
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

-- | Represents a result of an inline query that was chosen by the user and sent to their chat partner.
data ChosenInlineResult
  = ChosenInlineResult
      { -- | The unique identifier for the result that was chosen.
        chosenInlineResultResultId :: ChosenInlineResultId,
        -- | The user that chose the result.
        chosenInlineResultFrom :: User,
        -- | Sender location, only for bots that require user location.
        chosenInlineResultLocation :: Maybe Location,
        -- | Identifier of the sent inline message. Available only if there is an inline keyboard attached to the message. Will be also received in callback queries and can be used to edit the message.
        chosenInlineResultInlineMessageId :: Maybe Text,
        -- | The query that was used to obtain the result.
        chosenInlineResultQuery :: Text
      }
  deriving (Generic, Show)

type AnswerInlineQuery =
  "answerInlineQuery"
    :> ReqBody '[JSON] AnswerInlineRequest
    :> Post '[JSON] (Response Bool)

-- | Use this method to send answers to an inline query.
-- No more than 50 results per query are allowed.
answerInlineQuery :: AnswerInlineRequest -> ClientM (Response Bool)
answerInlineQuery = client (Proxy @AnswerInlineQuery)

-- | Answer on an inline query.
data AnswerInlineRequest
  = AnswerInlineRequest
      { -- | Unique identifier for the answered query.
        answerInlineQueryInlineQueryId :: InlineQueryId,
        -- | A JSON-serialized array of results for the inline query
        answerInlineQueryResults :: [InlineQueryResult],
        -- | 	The maximum amount of time in seconds that the result of the inline query may be cached on the server. Defaults to __300__.
        answerInlineQueryCacheTime :: Maybe Int32,
        -- | Pass True, if results may be cached on the server side only for the user that sent the query. By default, results may be returned to any user who sends the same query
        answerInlineQueryIsPersonal :: Maybe Bool,
        -- | Pass the offset that a client should send in the next query with the same text to receive more results. Pass an empty string if there are no more results or if you don't support pagination. Offset length can't exceed 64 bytes.
        answerInlineQueryNextOffset :: Maybe Int32
        -- -- |
        -- answerInlineQuerySwitchPmText :: Maybe Text,
        -- -- |
        -- answerInlineQuerySwitchPmParameter :: Maybe Text
      }
  deriving (Generic, Show)

-- | This object represents one result of an inline query.
data InlineQueryResult = Article InlineQueryResultArticle deriving (Generic, Show)

newtype InlineQueryResultId = InlineQueryResultId Int64
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

-- | Represents a link to an article or web page.
data InlineQueryResultArticle
  = InlineQueryResultArticle
      { -- | Type of the result, must be article.
        inlineQueryResultArticleType :: Text,
        -- | Unique identifier for this result, 1-64 Bytes.
        inlineQueryResultArticleId :: InlineQueryResultId,
        -- | Title of the result.
        inlineQueryResultArticleTitle :: Text,
        -- | Content of the message to be sent.
        inlineQueryResultArticleInputMessageContent :: InputMessageContent,
        -- | Inline keyboard attached to the message.
        inlineQueryResultArticleReplyMarkup :: Maybe InlineKeyboardMarkup,
        -- | URL of the result.
        inlineQueryResultArticleUrl :: Maybe Text,
        -- | Pass True, if you don't want the URL to be shown in the message.
        inlineQueryResultArticleHideUrl :: Maybe Bool,
        -- | Short description of the result.
        inlineQueryResultArticleDescription :: Maybe Text,
        -- | Url of the thumbnail for the result.
        inlineQueryResultArticleThumbUrl :: Maybe Text,
        -- | Thumbnail width.
        inlineQueryResultArticleThumbWidth :: Maybe Int32,
        -- | Thumbnail height.
        inlineQueryResultArticleThumbHeight :: Maybe Int32
      }
  deriving (Generic, Show)

data InputMessageContent = Text InputTextMessageContent
  deriving (Generic, Show)

-- | Represents the content of a text message to be sent as the result of an inline query.
data InputTextMessageContent
  = InputTextMessageContent
      { -- | Text of the message to be sent, 1-4096 characters.
        inputTextMessageContentMessageText :: Text
        --TODO
      }
  deriving (Generic, Show)

deriveJSON' ''InlineQuery

deriveJSON' ''ChosenInlineResult

deriveJSON' ''AnswerInlineRequest

deriveJSON' ''InlineQueryResult

deriveJSON' ''InputMessageContent

deriveJSON' ''InputTextMessageContent

deriveJSON' ''InlineQueryResultArticle
