ARG b='3.10'
ARG a="${b}.2"
ARG example=''

FROM alpine:"${a}"
ARG example
