<?php

class ConfigPreprocessor
{
    private $config_striped;

    private $config_ordered;

    public function __construct($config) {
        $this->config_striped = array();
        $this->walk($config);
    }

    private function walk(&$v) {
        if(is_array($v)) {
            $this->walk_arr($v);
        }
        else if(is_object($v)) {
            $this->walk_obj($v);
        }
    }

    private function walk_obj($v) {
        if($this->does_obj_contain_task($v)) {
            array_push($this->config_striped, $this->new_task_from_obj($v));
            $this->remove_task_from_obj($v);
            $this->walk($v);
        }
        else {
            foreach($v as $k => $r) {
                $this->walk($r);
            }
        }
    }

    private function walk_arr(&$v) {
        if($this->does_arr_contain_task($v)) {
            array_push($this->config_striped, $this->new_task_from_arr($v));
            $this->remove_task_from_arr($v);
            $this->walk($v);
        }        
        else {
            foreach($v as $k => $r) {
                $this->walk($r);
            }
        }
    }

    private function remove_task_from_obj($v) {
        unset($v->id);
        unset($v->command);
        unset($v->priority);
        unset($v->dependencies);
    }

    private function new_task_from_obj($v) {
        $t = new StdClass();
        $t->id = $v->id;
        $t->command = $v->command;
        $t->priority = $v->priority;
        $t->dependencies = $v->dependencies;
        $t->arr = false;
        return $t;
    }

    private function new_task_from_arr(&$v) {
        $t = new StdClass();
        $t->id = $v['id'];
        $t->command = $v['command'];
        $t->priority = $v['priority'];
        $t->dependencies = $v['dependencies'];
        $t->arr = true;
        return $t;
    }

    private function remove_task_from_arr(&$v) {
        unset($v['id']);
        unset($v['command']);
        unset($v['priority']);
        unset($v['dependencies']);
    }

    private function does_obj_contain_task($v) {
        return property_exists($v, 'id') and property_exists($v, 'command')
               and property_exists($v, 'priority') and property_exists($v, 'dependencies');
    }

    private function does_arr_contain_task($v) {
        return array_key_exists('id', $v) and array_key_exists('command', $v)
               and array_key_exists('priority', $v) and array_key_exists('dependencies', $v);
    }

    private function top_sort() {
        if(!isset($this->config_ordered)) {
            $this->config_ordered = array();
            $graph = $this->build_graph();
            while (!empty($graph)) {
                try {
                    $this->label_degs($graph);
                    $k = $this->get_node($graph);
                    array_push($this->config_ordered, $this->conv($graph[$k]));
                    unset($graph[$k]);
                }
                catch(Exception $e) {
                    throw $e;
                }
            }
        }
        else {
            return $this->config_ordered;
        }
        return $this->config_ordered;
    }

    private function conv($task) {
        unset($task->zero_deg);
        unset($task->next);
        if($task->arr) {
            unset($task->arr);
            $out = array();
            foreach($task as $propertyName => $propertyValue) {
                $out[$propertyName] = $propertyValue;
            }
            return $out;
        }
        else {
            unset($task->arr);
            return $task;
        }
    }

    private function get_node(&$graph) {
        foreach($graph as $k => $n) {
            if($n->zero_deg) {
                if(!isset($max_priority) || $n->priority > $max_priority) {
                    $key_out = $k;
                    $max_priority = $n->priority;
                }
            }
        }
        if(isset($key_out)) {
            return $key_out;
        }
        throw new Exception("Dependency cycle detected.");
    }

    private function label_degs(&$graph) {
        foreach($graph as $k => $n) {
            $n->zero_deg = true;
        }
        foreach($graph as $k => $n) {
            foreach($n->next as $k => $next) {
                $next->zero_deg = false;
            }
        }
    }
    private function build_graph() {
        $graph = array();
        $order = $this->order_from_dep();
        for($i = 0; $i < count($this->config_striped); ++$i) {
            array_push($graph, $this->config_striped[$i]);
            if(array_key_exists($graph[$i]->id, $order)) {
                $graph[$i]->next = $this->find_next($order[$graph[$i]->id]);
            }
            else {
                $graph[$i]->next = array();
            }
        }
        return $graph;
    }

    private function find_next(&$ids) {
        //TODO: build an array of objects from $ids
        $neighbors = array();
        foreach($ids as $k => $id) {
            array_push($neighbors, $this->find_by_id($id));
        }
        return $neighbors;
    }

    private function find_by_id(&$id) {
        for($i = 0; $i < count($this->config_striped); ++$i) {
            if($this->config_striped[$i]->id === $id) {
                return $this->config_striped[$i];
            }
        }
    }

    private function order_from_dep() {
        $order = array();
        for($i = 0; $i < count($this->config_striped); ++$i) {
            foreach($this->config_striped[$i]->dependencies as $k => $dep_id) {
                if(!array_key_exists($dep_id, $order)) {
                    $order[$dep_id] = array($this->config_striped[$i]->id);
                }
                else {
                    array_push($order[$dep_id], $this->config_striped[$i]->id);
                }
            }
        }
        return $order;
    }

    private function array_contains($assoc, $k, $v) {
        if(array_key_exists($k, $assoc)) {
            return in_array($v, $assoc[$k]);
        }
        return false;
    }

    /**
     * Get an array of tasks from the config in the right order.
     */
    public function getAllTasks() {
        try {
            return $this->top_sort();
        }
        catch(Exception $e) {
            throw $e;
        }
    }
}
