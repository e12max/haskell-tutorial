FROM postgres:latest

ENV POSTGRES_USER myuser
ENV POSTGRES_PASSWORD mypass

COPY docker-entrypoint-initdb.d/ ./docker-entrypoint-initdb.d/
