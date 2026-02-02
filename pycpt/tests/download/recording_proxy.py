import argparse
import functools
import hashlib
import http.server
from pathlib import Path
import re
from urllib.parse import urlparse
import urllib.request


class RecordingProxyHandler(http.server.BaseHTTPRequestHandler):
    def __init__(self, request, client_address, server, dir: str, is_record: bool):
        self.dir = Path(dir)
        self.is_record = is_record
        super().__init__(request, client_address, server)

    def do_GET(self) -> None:
        url = self.path
        fingerprint = hashlib.md5(url.encode()).hexdigest()
        parsed_url = urlparse(url)
        def sanitize(s):
            return re.sub(r'[^a-zA-Z0-9.-]', '_', s)
        subdir = sanitize(parsed_url.netloc)
        slug = sanitize(parsed_url.path[:100])
        file_path = self.dir / subdir / f"{slug}-{fingerprint}"

        if self.is_record:
            self._fetch_and_record(url, file_path)
        elif file_path.exists():
            self._serve_from_disk(file_path, url)
        else:
            self.send_error(404, f"No recorded response found for: {url}")

    def _serve_from_disk(self, path: Path, url: str) -> None:
        with path.open('rb') as f:
            content = f.read()
        self.send_response(200)
        self.end_headers()
        self.wfile.write(content)

    def _fetch_and_record(self, url: str, file_path: Path) -> None:
        try:
            with urllib.request.urlopen(url) as response:
                content = response.read()
            file_path.parent.mkdir(parents=True, exist_ok=True)
            with open(file_path, "wb") as f:
                f.write(content)
            self.send_response(200)
            self.end_headers()
            self.wfile.write(content)
        except Exception as e:
            self.send_error(502, f"Upstream Error: {e}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Recording HTTP proxy")
    parser.add_argument("--record", action="store_true",
                        help="Make real network requests and save the responses.")
    parser.add_argument("--port", type=int, default=8080, help="Proxy port (default: 8080)")
    parser.add_argument("--dir", default="responses", help="Directory to store responses")
    args = parser.parse_args()
    server = http.server.HTTPServer(
        ('127.0.0.1', args.port),
        functools.partial(
            RecordingProxyHandler, dir=args.dir, is_record=args.record
        )
    )
    mode = 'recording' if args.record else 'playback'
    print(f"Recording proxy: {mode} mode on port {args.port}")
    server.serve_forever()
