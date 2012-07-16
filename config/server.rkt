#hash((module . "flow.module")
      (server-port . 8000)
      (base-url . "http://localhost:8000")
      (config-url . "https://raw.github.com/craftofelectronics/flow/master/config/")
      (versions . (list 
                   (list "versions.rkt" 100)
                   (list "server.rkt" 100)
                   (list "arduino.rkt" 100)
                   (list "arduinouno.rkt" 100)))
       
      (libraries . (list 
                    "https://raw.github.com/craftofelectronics/flow/master/occam/flow/flow.module"))
      )
        