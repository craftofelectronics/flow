#hash((module . "flow.module")
      (server-port . 8000)
      (base-url . "http://localhost:8000")
      (remote-url . "https://raw.github.com/craftofelectronics/flow/master")
      (versions . #hash(("server.rkt"          . 100)
                        ("arduino.rkt"         . 100)
                        ("arduinouno.rkt"      . 100)
                        ("blocksets.rkt"       . 100)
                        ("flow.module"         . 101)
                        ))
      (update-paths . #hash(("server.rkt"      . "config")
                            ("arduino.rkt"     . "config")
                            ("arduinouno.rkt"  . "config")
                            ("blocksets.rkt"   . "config")
                            ("flow.module"     . "occam/flow")
                            ))
      (update-urls . #hash(("occam/flow" .
                            "https://raw.github.com/craftofelectronics/flow/master/occam/flow/")
                           ("blocks" .
                            "https://raw.github.com/craftofelectronics/flow/master/interface/blocks/")
                           ("config" .
                            "https://raw.github.com/craftofelectronics/flow/master/config/")))
      
      (max-windows-com-port . 15)
      )
        
