FROM fpco/stack-build:lts-12.16
LABEL Author Kyle Isom

VOLUME	/whereami
ADD . 	/whereami
WORKDIR /whereami

RUN stack build

ENTRYPOINT [ "./run.sh" ]
CMD [ "run" ]
