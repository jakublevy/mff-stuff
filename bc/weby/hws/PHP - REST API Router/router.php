<?php

class Router
{
    private function regexValidate(string &$action) {
        if(preg_match('#(?<path>([a-zA-Z_]+/)+)(?<method>[a-zA-Z_]+)#', $action, $matches)) {
            return [ 'path' => $matches['path'], 'method' => $matches['method'], 'controller' => substr($matches[2], 0, -1) ];
        }
        return false;
    }

    private function getMethod(array &$matches) {
        $controllerFile =  substr(__DIR__ . '/controllers/' . $matches['path'], 0, -1) . '.php';
        if(file_exists($controllerFile)) {
            require($controllerFile);
            $className = $matches['controller'] . 'Controller';
            if(class_exists($className)) {
                $class = new ReflectionClass($className);
                $instance = $class->newInstanceArgs();
                // $methodName = 'get' . $matches['method'];
                if($_SERVER['REQUEST_METHOD'] === 'GET') {
                    $methodName = 'get' . $matches['method'];
                }
                else if($_SERVER['REQUEST_METHOD'] === 'POST') {
                    $methodName = 'post' . $matches['method'];
                }
                else {
                    return false;
                }
                if(method_exists($instance, $methodName)) {
                    $method = $class->getMethod($methodName);
                    return [ 'instance' => $instance, 'class' => $class, 'method' => $method ];
                }
                else {
                    return false;
                }
            }
            else {
                return false;
            }
        }
        else {
            return false;
        }
    }

    public function dispatch() {
        if($_SERVER['REQUEST_METHOD'] === 'GET') {
            $data = $_GET;
        }
        else if($_SERVER['REQUEST_METHOD'] === 'POST') {
            $data = $_POST;
        }
        else {
            http_response_code(400);
        }

        if(isset($_GET['action'])) {
            $actionGroups = $this->regexValidate($_GET['action']);
            if($actionGroups !== false) {
                $methodInfo = $this->getMethod($actionGroups);
                if($methodInfo !== false) {
                    // if(isset($data)) {
                        $parameters = $methodInfo['method']->getParameters();
                        $toPass = array();
                        $ok = true;
                        foreach($parameters as $p) {
                            $pName = $p->getName();
                            if(isset($data[$pName])) {
                                $toPass[$pName] = htmlspecialchars($data[$pName]);
                            }
                            else {
                                http_response_code(400);
                                $ok = false;
                            }
                        }
                        if($ok) {
                            try {
                                $ret = $methodInfo['method']->invokeArgs($methodInfo['instance'], $toPass);
                                if($ret === null) {
                                    http_response_code(204);
                                }
                                else {
                                    echo(json_encode($ret, JSON_PRETTY_PRINT));
                                }
                            }
                            catch (Exception $e) {
                                http_response_code(500);
                            }
                        }
                    // }
                }
                else {
                    http_response_code(404);
                }
            }
            else {
                http_response_code(400);
            }
        }
        else {
            http_response_code(400);
        }
    }	
}
