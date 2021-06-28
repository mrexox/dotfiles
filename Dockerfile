FROM scratch as builder

WORKDIR /app
COPY .bashrc .vimrc .

FROM alpine

COPY --from=builder \
  /app/.bashrc /app/.vimrc .
