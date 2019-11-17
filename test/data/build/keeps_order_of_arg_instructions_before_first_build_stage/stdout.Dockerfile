ARG b='3.10'
ARG a="${b}.3"

FROM alpine:"${a}"
ARG example
