#!/usr/bin/env python3
"""Export private homepage pageviews with the active gcloud account."""

import argparse
import csv
import json
import os
import re
import ssl
import subprocess
import sys
import tempfile
from datetime import date
from pathlib import Path
from urllib.error import HTTPError
from urllib.request import Request, urlopen


PROJECT = "portfolio-blog-f49c1"
QUERY_URL = (
    f"https://firestore.googleapis.com/v1/projects/{PROJECT}/databases/"
    "(default)/documents:runQuery"
)
COLUMNS = ("visited_at", "path", "country", "region", "latitude", "longitude")
PAGE_SIZE = 500


def month_bounds(value):
    if not re.fullmatch(r"[0-9]{4}-[0-9]{2}", value):
        raise argparse.ArgumentTypeError("Use a UTC month in YYYY-MM format.")
    year, month = map(int, value.split("-"))
    try:
        start = date(year, month, 1)
        end = date(year + month // 12, month % 12 + 1, 1)
    except ValueError as exc:
        raise argparse.ArgumentTypeError("Enter a valid UTC month.") from exc
    return [f"{day.isoformat()}T00:00:00Z" for day in (start, end)]


def read_page(token, body):
    trust = ssl.create_default_context()
    if sys.platform == "darwin" and not trust.get_ca_certs() and Path("/etc/ssl/cert.pem").is_file():
        trust.load_verify_locations("/etc/ssl/cert.pem")
    request = Request(
        QUERY_URL,
        data=json.dumps(body).encode("utf-8"),
        headers={
            "Authorization": f"Bearer {token}",
            "Content-Type": "application/json",
            "X-Goog-User-Project": PROJECT,
        },
    )
    try:
        with urlopen(request, timeout=60, context=trust) as response:
            return json.load(response)
    except HTTPError as exc:
        raise RuntimeError(f"Firestore query failed (HTTP {exc.code}).") from exc


def export_rows(writer, token, bounds):
    query = {
        "from": [{"collectionId": "homepage_pageviews"}],
        "select": {"fields": [{"fieldPath": name} for name in COLUMNS]},
        "orderBy": [
            {"field": {"fieldPath": name}, "direction": "ASCENDING"}
            for name in ("visited_at", "__name__")
        ],
        "limit": PAGE_SIZE,
    }
    if bounds:
        for key, bound in zip(("startAt", "endAt"), bounds):
            # Begin just before the first instant, end just before the next month.
            query[key] = {"values": [{"timestampValue": bound}], "before": True}

    body = {"structuredQuery": query}
    total = 0
    while True:
        page = read_page(token, body)
        if not isinstance(page, list) or not page:
            raise ValueError("Firestore returned an invalid query response.")
        docs = []
        for result in page:
            if "error" in result:
                raise RuntimeError("Firestore returned an incomplete query.")
            if "document" in result:
                docs.append(result["document"])

        for doc in docs:
            fields = doc["fields"]
            row = [fields["visited_at"]["timestampValue"], fields["path"]["stringValue"]]
            for column in COLUMNS[2:]:
                cell = fields.get(column, {"nullValue": None})
                if "nullValue" in cell:
                    row.append("")
                elif column in ("country", "region"):
                    row.append(cell["stringValue"])
                else:
                    key = "doubleValue" if "doubleValue" in cell else "integerValue"
                    row.append(cell[key])
            writer.writerow(row)
        total += len(docs)
        if len(docs) < PAGE_SIZE:
            return total

        # Keep the same snapshot and break timestamp ties with the document name.
        if "readTime" not in body:
            body["readTime"] = page[-1]["readTime"]
        last = docs[-1]
        cursor = {
            "values": [last["fields"]["visited_at"], {"referenceValue": last["name"]}],
            "before": False,
        }
        if query.get("startAt") == cursor:
            raise RuntimeError("Firestore returned a repeated page.")
        query["startAt"] = cursor


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path, help="New private CSV file to create")
    parser.add_argument("--month", metavar="YYYY-MM", type=month_bounds,
                        help="Export only this UTC calendar month (default: all history)")
    parser.add_argument("--account", help="Google Cloud account to use (default: active account)")
    args = parser.parse_args()
    temporary = None
    try:
        if args.output.exists():
            raise FileExistsError(f"Output already exists: {args.output}")
        login = ["gcloud", "auth", "print-access-token", "--quiet"]
        if args.account:
            login.extend(["--account", args.account])
        token = subprocess.run(
            login,
            check=True, capture_output=True, text=True, timeout=60,
        ).stdout.strip()
        if not token:
            raise RuntimeError("gcloud returned an empty access token.")
        with tempfile.NamedTemporaryFile(
            mode="w", encoding="utf-8", newline="", delete=False, dir=args.output.parent
        ) as out:
            temporary = Path(out.name)
            writer = csv.writer(out)
            writer.writerow(COLUMNS)
            total = export_rows(writer, token, args.month)
        # Publish only a complete private file. A concurrent export cannot overwrite it.
        os.link(temporary, args.output)
    except subprocess.CalledProcessError:
        parser.exit(1, "Could not authenticate. Sign in with gcloud auth login and retry.\n")
    except (KeyError, TypeError, ValueError, OSError, RuntimeError,
            subprocess.TimeoutExpired) as exc:
        parser.exit(1, f"Export failed: {exc}\n")
    finally:
        if temporary is not None:
            temporary.unlink(missing_ok=True)
    print(f"Exported {total} pageviews to {args.output}")


if __name__ == "__main__":
    main()
