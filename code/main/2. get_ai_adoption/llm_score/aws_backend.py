"""Public AWS Bedrock invocation helpers.

The functions in this module use boto3's standard credential and region
resolution.  They deliberately expose the frozen generation controls at the
infrastructure boundary so callers never depend on an institution-specific
model wrapper.
"""

from __future__ import annotations

from concurrent.futures import ThreadPoolExecutor
from typing import Any, Iterable, Optional, Sequence


def create_bedrock_client(*, region_name: Optional[str] = None) -> Any:
    """Create a standard boto3 Bedrock Runtime client."""

    import boto3

    kwargs = {"region_name": region_name} if region_name else {}
    return boto3.client("bedrock-runtime", **kwargs)


def _response_text(response: Any) -> str:
    """Extract text from a successful Bedrock Converse response."""

    try:
        content: Sequence[dict[str, Any]] = response["output"]["message"]["content"]
    except (KeyError, TypeError) as exc:
        raise RuntimeError(
            "AWS Bedrock returned a response without output.message.content."
        ) from exc

    text = "".join(
        str(block.get("text", ""))
        for block in content
        if isinstance(block, dict) and "text" in block
    ).strip()
    if not text:
        raise RuntimeError("AWS Bedrock returned no text content.")
    return text


def invoke_bedrock_prompt(
    prompt: str,
    *,
    model_id: str,
    region_name: Optional[str] = None,
    temperature: Optional[float] = 0.0,
    max_new_tokens: int = 8,
    client: Any = None,
) -> str:
    """Invoke one prompt through the standard Bedrock Converse API.

    boto3 resolves credentials through its normal chain, including configured
    AWS profiles, environment-based credentials, IAM roles, and AWS SSO.
    """

    if temperature is None:
        raise ValueError(
            "A temperature is required by the frozen research configuration."
        )
    if max_new_tokens <= 0:
        raise ValueError("max_new_tokens must be a positive integer.")

    bedrock = client or create_bedrock_client(region_name=region_name)
    response = bedrock.converse(
        modelId=model_id,
        messages=[{"role": "user", "content": [{"text": prompt}]}],
        inferenceConfig={
            "temperature": float(temperature),
            "maxTokens": int(max_new_tokens),
        },
    )
    return _response_text(response)


def invoke_bedrock_prompts(
    prompts: Iterable[tuple[str, str]],
    *,
    model_id: str,
    region_name: Optional[str] = None,
    temperature: Optional[float] = 0.0,
    max_new_tokens: int = 8,
    max_workers: int = 5,
    client: Any = None,
) -> Iterable[tuple[str, dict[str, Any]]]:
    """Invoke prompt pairs concurrently using the existing bulk-result contract."""

    if max_workers <= 0:
        raise ValueError("max_workers must be a positive integer.")
    bedrock = client or create_bedrock_client(region_name=region_name)

    def invoke_one(item: tuple[str, str]) -> tuple[str, dict[str, Any]]:
        identifier, prompt = item
        try:
            text = invoke_bedrock_prompt(
                prompt,
                model_id=model_id,
                region_name=region_name,
                temperature=temperature,
                max_new_tokens=max_new_tokens,
                client=bedrock,
            )
            return identifier, {"model_response_string": text}
        except Exception as exc:
            return identifier, {"error": str(exc)}

    with ThreadPoolExecutor(max_workers=max_workers) as executor:
        yield from executor.map(invoke_one, prompts)
