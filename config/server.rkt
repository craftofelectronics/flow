#hash((module . "flow.module")
      (server-port . 8000)
      (base-url . "http://localhost:8000")
      (remote-url . "https://raw.github.com/craftofelectronics/flow/master")
      (versions . (("server.rkt"      "config"       100)
                   ("arduino.rkt"     "config"       100)
                   ("arduinouno.rkt"  "config"       100)
                   ("flow.module"     "occam/flow"   101)
                   ("ifttt.js"        "interface"    100)
                   ))
      (update-paths . (("occam/flow"
                        "https://raw.github.com/craftofelectronics/flow/master/occam/flow/")
                       ("interface"
                        "https://raw.github.com/craftofelectronics/flow/master/interface/")
                       ("config"
                        "https://raw.github.com/craftofelectronics/flow/master/config/")))
      (max-windows-com-port . 15)
      )
        
