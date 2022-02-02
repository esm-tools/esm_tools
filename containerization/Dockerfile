# syntax=docker/dockerfile:1

# The following can result in a much smaller container, but it currently does not work.
# See here: https://github.com/autamus/registry/issues
# FROM ghcr.io/autamus/python:latest

FROM python:3.9-slim-bullseye

WORKDIR /app

COPY . .
RUN python3 -m pip install .

ENTRYPOINT ["./containerization/entrypoint.sh"]
