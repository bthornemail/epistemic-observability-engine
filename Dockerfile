FROM racket/racket:8.12-full

WORKDIR /app

# Copy package files
COPY *.rkt *.md LICENSE ./
COPY substrate-* ./substrate-*/
COPY rpc ./rpc/
COPY utils ./utils/
COPY tests ./tests/

# Install dependencies and build
RUN raco pkg install --auto base rackunit-lib math-lib net-lib web-server-lib json crypto-lib && \
    raco make . && \
    raco test tests/

# Expose RPC port
EXPOSE 8080

# Run the server
CMD ["racket", "main.rkt"]






