#hash((module . "flow.module")
      (tvm-installed . 0)
      (server-port . 8000)
      (base-url . "http://localhost:8000")
      (remote-url . "https://raw.github.com/craftofelectronics/flow/master")
      (versions . #hash(("server.rkt"          . 101)
                        ("arduino.rkt"         . 100)
                        ("arduinouno.rkt"      . 100)
                        
                        ("flow.module"         . 101)
                        ("flow.js"             . 100)
                        
                        ("fading.module"       . 100)
                        ("fading.js"           . 100)
                        
                        ("sensing-light.module"       . 100)
                        ("sensing-light.js"           . 100)
                        
                        ("temp-logging.module"       . 101)
                        ("temp-logging.js"           . 100)
                        
                        ))
      
      (update-paths . #hash(("server.rkt"      . "config")
                            ("arduino.rkt"     . "config")
                            ("arduinouno.rkt"  . "config")
                            
                            ("flow.module"     . "occam/flow")
                            ("flow.js"         . "interface/blocks")
                            
                            ("fading.module"     . "occam/flow")
                            ("fading.js"         . "interface/blocks")
                            
                            ("sensing-light.module"     . "occam/flow")
                            ("sensing-light.js"         . "interface/blocks")
                            
                            ("temp-logging.module"     . "occam/flow")
                            ("temp-logging.js"         . "interface/blocks")
                            ))
      (blocksets 
       . #hash(
               ("Fading" . #hash(("filename"    . "fading.js")
                                 ("sort"        . 0)))
               ("Flow"   . #hash(("filename"    . "flow.js")
                                 ("sort"        . 10)))
               ("Sensing Light"   . #hash(("filename"    . "sensing-light.js")
                                          ("sort"        . 20)))
               ("Logging Temperature"   . #hash(("filename"    . "temp-logging.js")
                                                ("sort"        . 30)))
               ))
      
      (update-urls . #hash(("occam/flow" .
                                         "https://raw.github.com/craftofelectronics/flow/master/occam/flow/")
                           ("interface/blocks" .
                                               "https://raw.github.com/craftofelectronics/flow/master/interface/blocks/")
                           ("config" .
                                     "https://raw.github.com/craftofelectronics/flow/master/config/")))
      
      (max-windows-com-port . 15)
      )

