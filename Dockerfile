FROM pbrisbin/heroku-haskell-stack:1.0.2

COPY . /app/user
RUN stack install

RUN rm -rf /app/user/.stack-work
