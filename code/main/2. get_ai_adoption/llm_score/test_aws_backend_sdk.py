"""Offline contract checks against the actual boto3 service model."""

from __future__ import annotations

import sys
import unittest
from pathlib import Path

try:
    import boto3
    from botocore.stub import Stubber
except ImportError:
    boto3 = None

sys.path.insert(0, str(Path(__file__).resolve().parent))
import aws_backend


@unittest.skipIf(boto3 is None, "Install the scoring requirements to test the real AWS SDK")
class BedrockSdkTests(unittest.TestCase):
    def test_real_sdk_accepts_frozen_converse_request_without_network(self):
        client = boto3.client(
            "bedrock-runtime", region_name="eu-west-2",
            aws_access_key_id="testing", aws_secret_access_key="testing",
        )
        expected = {
            "modelId": "eu.anthropic.claude-sonnet-4-6",
            "messages": [{"role": "user", "content": [{"text": "test evidence"}]}],
            "inferenceConfig": {"temperature": 0.0, "maxTokens": 8},
        }
        response = {
            "output": {"message": {"role": "assistant", "content": [{"text": "2"}]}},
            "stopReason": "end_turn",
            "usage": {"inputTokens": 10, "outputTokens": 1, "totalTokens": 11},
            "metrics": {"latencyMs": 1},
        }
        with Stubber(client) as stubber:
            stubber.add_response("converse", response, expected)
            self.assertEqual(aws_backend.invoke_bedrock_prompt(
                "test evidence", model_id=expected["modelId"], client=client,
            ), "2")
            stubber.assert_no_pending_responses()


if __name__ == "__main__":
    unittest.main()
