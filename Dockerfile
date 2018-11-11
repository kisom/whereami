FROM fpco/stack-build:lts-12.16
LABEL Author Kyle Isom

RUN mkdir /whereami
ADD . 	/whereami
WORKDIR /whereami
RUN stack build

ENTRYPOINT [ "stack" ]
CMD [ "run" ]