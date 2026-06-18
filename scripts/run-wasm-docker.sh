#!/bin/sh
docker run -p 8080:8080 waterpark bash -c  "cd /opencascade-hs/playground/dist && python3 -m http.server 8080"
