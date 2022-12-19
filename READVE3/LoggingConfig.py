from logging import config

log_config = {
    "version":1,
    "root":{
        "handlers" : ["console","file"],
        "level": "DEBUG"
    },
    "handlers":{
        "console":{
            "formatter": "short std_out",
            "class": "logging.StreamHandler",
            "level": "DEBUG"
        },
        "file":{
            "formatter": "short std_out",
            "class": "logging.StreamHandler",
            'stream': open('./READVE3.log','w'),
            "level": "DEBUG"
        }
    },
    "formatters":{
        "verbose std_out": {
            # "format": "%(asctime)s : %(levelname)s : %(module)s : %(funcName)s : %(lineno)d : (Process Details : (%(process)d, %(processName)s), Thread Details : (%(thread)d, %(threadName)s))\nLog : %(message)s",
            "format": "%(asctime)s : %(levelname)s : %(module)s : %(funcName)s : %(lineno)d : (Process Details : (%(process)d, %(processName)s), Thread Details : (%(thread)d, %(threadName)s))Log : %(message)s",
            "datefmt":"%d-%m-%Y %I:%M:%S"
        },
        "short std_out": {
            "format": "Log %(levelname)s: %(funcName)s:%(lineno)d %(message)s",
            "datefmt":"%d-%m-%Y %I:%M:%S"
        }
    },
}

config.dictConfig(log_config)
