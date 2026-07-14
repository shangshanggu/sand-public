"""Lightweight YAML loader shared across setup scripts.

This module exposes :func:`load_yaml_minimal`, a dependency-free parser for the
restricted YAML subset used by the thesis reproduction configuration files.
It was extracted from ``scripts/00_setup/validate_config.py`` so other helpers
(such as checksum generation) can reuse the same logic without duplicating
parsing code.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List

__all__ = ["load_yaml_minimal"]


@dataclass
class Context:
    """State for parsing the minimal YAML subset used in configuration files."""

    indent: int
    kind: str  # "dict", "list", or "pending"
    container: Any
    parent: Dict[str, Any] | None = None
    key: str | None = None


def _strip_comment(line: str) -> str:
    in_single = False
    in_double = False
    for index, char in enumerate(line):
        if char == "'" and not in_double:
            in_single = not in_single
        elif char == '"' and not in_single:
            in_double = not in_double
        elif char == "#" and not in_single and not in_double:
            return line[:index]
    return line


def _parse_scalar(token: str) -> Any:
    token = token.strip()
    if not token:
        return ""
    lowered = token.lower()
    if lowered == "null":
        return None
    if lowered == "true":
        return True
    if lowered == "false":
        return False
    if token.isdigit() or (token.startswith("-") and token[1:].isdigit()):
        try:
            return int(token)
        except ValueError:
            pass
    try:
        if "." in token or "e" in lowered or "E" in token:
            return float(token)
    except ValueError:
        pass
    if (token.startswith('"') and token.endswith('"')) or (
        token.startswith("'") and token.endswith("'")
    ):
        return token[1:-1]
    return token


def load_yaml_minimal(path: Path) -> Any:
    """Load a YAML file using a small, dependency-free subset parser."""

    root: Dict[str, Any] = {}
    stack: List[Context] = [Context(indent=-1, kind="dict", container=root)]

    lines = path.read_text(encoding="utf-8").splitlines()
    for line_number, raw in enumerate(lines, start=1):
        cleaned = _strip_comment(raw).rstrip()
        if not cleaned.strip():
            continue

        indent = len(cleaned) - len(cleaned.lstrip(" "))
        content = cleaned.strip()

        while len(stack) > 1 and indent <= stack[-1].indent:
            context = stack.pop()
            if context.kind == "pending":
                context.parent[context.key] = {}

        top = stack[-1]

        if top.kind == "pending" and indent > top.indent:
            if content.startswith("- "):
                new_container: Any = []
                top.parent[top.key] = new_container
                stack[-1] = Context(indent=top.indent, kind="list", container=new_container)
                top = stack[-1]
            else:
                new_container = {}
                top.parent[top.key] = new_container
                stack[-1] = Context(indent=top.indent, kind="dict", container=new_container)
                top = stack[-1]

        if content.startswith("-"):
            if top.kind == "pending":
                new_container = []
                top.parent[top.key] = new_container
                stack[-1] = Context(indent=top.indent, kind="list", container=new_container)
                top = stack[-1]
            if top.kind != "list":
                raise ValueError(
                    f"Line {line_number}: list item without a parent list context"
                )
            value_token = content[1:].strip()
            if value_token:
                top.container.append(_parse_scalar(value_token))
            else:
                new_dict: Dict[str, Any] = {}
                top.container.append(new_dict)
                stack.append(Context(indent=indent, kind="dict", container=new_dict))
            continue

        if ":" in content:
            key, value = content.split(":", 1)
            key = key.strip()
            value = value.strip()
            if top.kind == "pending":
                new_dict = {}
                top.parent[top.key] = new_dict
                stack[-1] = Context(indent=top.indent, kind="dict", container=new_dict)
                top = stack[-1]
            if top.kind != "dict":
                raise ValueError(
                    f"Line {line_number}: mapping entry encountered outside a mapping"
                )
            if value:
                top.container[key] = _parse_scalar(value)
            else:
                stack.append(
                    Context(
                        indent=indent,
                        kind="pending",
                        container=None,
                        parent=top.container,
                        key=key,
                    )
                )
            continue

        raise ValueError(f"Line {line_number}: unsupported YAML construct '{content}'")

    for context in stack:
        if context.kind == "pending":
            context.parent[context.key] = {}
    return root
