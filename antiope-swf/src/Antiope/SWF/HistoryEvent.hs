{-# LANGUAGE OverloadedStrings #-}

module Antiope.SWF.HistoryEvent
  ( historyEventToText
  ) where

import Control.Applicative
import Control.Lens
import Data.Monoid         ((<>))
import Data.Text           (Text)

import qualified Data.Text             as T
import qualified Network.AWS.SWF.Types as SWF

historyEventToText :: SWF.HistoryEvent -> Text
historyEventToText e = "HistoryEvent"
  <> " { " <> maybe "" id eaText
  <>   " heEventTimestamp = "  <> T.pack (show (e ^. SWF.heEventTimestamp ))
  <> " , heEventType = "       <> T.pack (show (e ^. SWF.heEventType      ))
  <> " , heEventId = "         <> T.pack (show (e ^. SWF.heEventId        ))
  <> " }"
  where eaText = Nothing
          <|> fmap (("heWorkflowExecutionCancelRequestedEventAttributes = "                 <>) . T.pack . show) (e ^. SWF.heWorkflowExecutionCancelRequestedEventAttributes)
          <|> fmap (("heRecordMarkerFailedEventAttributes = "                               <>) . T.pack . show) (e ^. SWF.heRecordMarkerFailedEventAttributes)
          <|> fmap (("heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes = "  <>) . T.pack . show) (e ^. SWF.heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes)
          <|> fmap (("heLambdaFunctionStartedEventAttributes = "                            <>) . T.pack . show) (e ^. SWF.heLambdaFunctionStartedEventAttributes)
          <|> fmap (("heDecisionTaskScheduledEventAttributes = "                            <>) . T.pack . show) (e ^. SWF.heDecisionTaskScheduledEventAttributes)
          <|> fmap (("heWorkflowExecutionCompletedEventAttributes = "                       <>) . T.pack . show) (e ^. SWF.heWorkflowExecutionCompletedEventAttributes)
          <|> fmap (("heStartTimerFailedEventAttributes = "                                 <>) . T.pack . show) (e ^. SWF.heStartTimerFailedEventAttributes)
          <|> fmap (("heActivityTaskScheduledEventAttributes = "                            <>) . T.pack . show) (e ^. SWF.heActivityTaskScheduledEventAttributes)
          <|> fmap (("heScheduleActivityTaskFailedEventAttributes = "                       <>) . T.pack . show) (e ^. SWF.heScheduleActivityTaskFailedEventAttributes)
          <|> fmap (("heChildWorkflowExecutionCompletedEventAttributes = "                  <>) . T.pack . show) (e ^. SWF.heChildWorkflowExecutionCompletedEventAttributes)
          <|> fmap (("heMarkerRecordedEventAttributes = "                                   <>) . T.pack . show) (e ^. SWF.heMarkerRecordedEventAttributes)
          <|> fmap (("heScheduleLambdaFunctionFailedEventAttributes = "                     <>) . T.pack . show) (e ^. SWF.heScheduleLambdaFunctionFailedEventAttributes)
          <|> fmap (("heCompleteWorkflowExecutionFailedEventAttributes = "                  <>) . T.pack . show) (e ^. SWF.heCompleteWorkflowExecutionFailedEventAttributes)
          <|> fmap (("heLambdaFunctionCompletedEventAttributes = "                          <>) . T.pack . show) (e ^. SWF.heLambdaFunctionCompletedEventAttributes)
          <|> fmap (("heRequestCancelExternalWorkflowExecutionFailedEventAttributes = "     <>) . T.pack . show) (e ^. SWF.heRequestCancelExternalWorkflowExecutionFailedEventAttributes)
          <|> fmap (("heTimerCanceledEventAttributes = "                                    <>) . T.pack . show) (e ^. SWF.heTimerCanceledEventAttributes)
          <|> fmap (("heWorkflowExecutionStartedEventAttributes = "                         <>) . T.pack . show) (e ^. SWF.heWorkflowExecutionStartedEventAttributes)
          <|> fmap (("heActivityTaskCompletedEventAttributes = "                            <>) . T.pack . show) (e ^. SWF.heActivityTaskCompletedEventAttributes)
          <|> fmap (("heDecisionTaskTimedOutEventAttributes = "                             <>) . T.pack . show) (e ^. SWF.heDecisionTaskTimedOutEventAttributes)
          <|> fmap (("heCancelTimerFailedEventAttributes = "                                <>) . T.pack . show) (e ^. SWF.heCancelTimerFailedEventAttributes)
          <|> fmap (("heChildWorkflowExecutionStartedEventAttributes = "                    <>) . T.pack . show) (e ^. SWF.heChildWorkflowExecutionStartedEventAttributes)
          <|> fmap (("heActivityTaskCanceledEventAttributes = "                             <>) . T.pack . show) (e ^. SWF.heActivityTaskCanceledEventAttributes)
          <|> fmap (("heActivityTaskTimedOutEventAttributes = "                             <>) . T.pack . show) (e ^. SWF.heActivityTaskTimedOutEventAttributes)
          <|> fmap (("heDecisionTaskStartedEventAttributes = "                              <>) . T.pack . show) (e ^. SWF.heDecisionTaskStartedEventAttributes)
          <|> fmap (("heWorkflowExecutionTerminatedEventAttributes = "                      <>) . T.pack . show) (e ^. SWF.heWorkflowExecutionTerminatedEventAttributes)
          <|> fmap (("heChildWorkflowExecutionCanceledEventAttributes = "                   <>) . T.pack . show) (e ^. SWF.heChildWorkflowExecutionCanceledEventAttributes)
          <|> fmap (("heRequestCancelActivityTaskFailedEventAttributes = "                  <>) . T.pack . show) (e ^. SWF.heRequestCancelActivityTaskFailedEventAttributes)
          <|> fmap (("heLambdaFunctionScheduledEventAttributes = "                          <>) . T.pack . show) (e ^. SWF.heLambdaFunctionScheduledEventAttributes)
          <|> fmap (("heChildWorkflowExecutionTimedOutEventAttributes = "                   <>) . T.pack . show) (e ^. SWF.heChildWorkflowExecutionTimedOutEventAttributes)
          <|> fmap (("heCancelWorkflowExecutionFailedEventAttributes = "                    <>) . T.pack . show) (e ^. SWF.heCancelWorkflowExecutionFailedEventAttributes)
          <|> fmap (("heStartChildWorkflowExecutionInitiatedEventAttributes = "             <>) . T.pack . show) (e ^. SWF.heStartChildWorkflowExecutionInitiatedEventAttributes)
          <|> fmap (("heSignalExternalWorkflowExecutionFailedEventAttributes = "            <>) . T.pack . show) (e ^. SWF.heSignalExternalWorkflowExecutionFailedEventAttributes)
          <|> fmap (("heActivityTaskStartedEventAttributes = "                              <>) . T.pack . show) (e ^. SWF.heActivityTaskStartedEventAttributes)
          <|> fmap (("heStartLambdaFunctionFailedEventAttributes = "                        <>) . T.pack . show) (e ^. SWF.heStartLambdaFunctionFailedEventAttributes)
          <|> fmap (("heChildWorkflowExecutionTerminatedEventAttributes = "                 <>) . T.pack . show) (e ^. SWF.heChildWorkflowExecutionTerminatedEventAttributes)
          <|> fmap (("heLambdaFunctionFailedEventAttributes = "                             <>) . T.pack . show) (e ^. SWF.heLambdaFunctionFailedEventAttributes)
          <|> fmap (("heWorkflowExecutionCanceledEventAttributes = "                        <>) . T.pack . show) (e ^. SWF.heWorkflowExecutionCanceledEventAttributes)
          <|> fmap (("heTimerStartedEventAttributes = "                                     <>) . T.pack . show) (e ^. SWF.heTimerStartedEventAttributes)
          <|> fmap (("heActivityTaskCancelRequestedEventAttributes = "                      <>) . T.pack . show) (e ^. SWF.heActivityTaskCancelRequestedEventAttributes)
          <|> fmap (("heWorkflowExecutionTimedOutEventAttributes = "                        <>) . T.pack . show) (e ^. SWF.heWorkflowExecutionTimedOutEventAttributes)
          <|> fmap (("heWorkflowExecutionSignaledEventAttributes = "                        <>) . T.pack . show) (e ^. SWF.heWorkflowExecutionSignaledEventAttributes)
          <|> fmap (("heTimerFiredEventAttributes = "                                       <>) . T.pack . show) (e ^. SWF.heTimerFiredEventAttributes)
          <|> fmap (("heActivityTaskFailedEventAttributes = "                               <>) . T.pack . show) (e ^. SWF.heActivityTaskFailedEventAttributes)
          <|> fmap (("heExternalWorkflowExecutionSignaledEventAttributes = "                <>) . T.pack . show) (e ^. SWF.heExternalWorkflowExecutionSignaledEventAttributes)
          <|> fmap (("heDecisionTaskCompletedEventAttributes = "                            <>) . T.pack . show) (e ^. SWF.heDecisionTaskCompletedEventAttributes)
          <|> fmap (("heStartChildWorkflowExecutionFailedEventAttributes = "                <>) . T.pack . show) (e ^. SWF.heStartChildWorkflowExecutionFailedEventAttributes)
          <|> fmap (("heChildWorkflowExecutionFailedEventAttributes = "                     <>) . T.pack . show) (e ^. SWF.heChildWorkflowExecutionFailedEventAttributes)
          <|> fmap (("heFailWorkflowExecutionFailedEventAttributes = "                      <>) . T.pack . show) (e ^. SWF.heFailWorkflowExecutionFailedEventAttributes)
          <|> fmap (("heContinueAsNewWorkflowExecutionFailedEventAttributes = "             <>) . T.pack . show) (e ^. SWF.heContinueAsNewWorkflowExecutionFailedEventAttributes)
          <|> fmap (("heSignalExternalWorkflowExecutionInitiatedEventAttributes = "         <>) . T.pack . show) (e ^. SWF.heSignalExternalWorkflowExecutionInitiatedEventAttributes)
          <|> fmap (("heLambdaFunctionTimedOutEventAttributes = "                           <>) . T.pack . show) (e ^. SWF.heLambdaFunctionTimedOutEventAttributes)
          <|> fmap (("heWorkflowExecutionFailedEventAttributes = "                          <>) . T.pack . show) (e ^. SWF.heWorkflowExecutionFailedEventAttributes)
          <|> fmap (("heWorkflowExecutionContinuedAsNewEventAttributes = "                  <>) . T.pack . show) (e ^. SWF.heWorkflowExecutionContinuedAsNewEventAttributes)
          <|> fmap (("heExternalWorkflowExecutionCancelRequestedEventAttributes = "         <>) . T.pack . show) (e ^. SWF.heExternalWorkflowExecutionCancelRequestedEventAttributes)
