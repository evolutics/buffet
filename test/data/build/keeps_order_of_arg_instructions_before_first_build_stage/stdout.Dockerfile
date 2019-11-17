ARG b='3.10'
ARG a="${b}.3"
ARG example=''

FROM alpine:"${a}"
ARG example
