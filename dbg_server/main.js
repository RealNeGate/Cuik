var socket = null;
var logs = "";
var funcs = [];
var func_cnt = 0;

var selected_pass = 0;
var old_selected = null;

function startup() {
    // reset state
    logs = "";
    funcs = [];
    document.getElementById("log").innerText = "";

    socket = new WebSocket("ws://localhost:8000");
    socket.onopen = () => {
        console.log('WebSocket connection opened');
        document.getElementById("connect_btn").disabled = true;
    };

    socket.onclose = () => {
        close();
        document.getElementById("connect_btn").disabled = false;
    }
    socket.onerror = (error) => {
        console.error('WebSocket error:', error);
    };

    socket.onmessage = (event) => {
        const new_msgs = event.data;
        // console.log("Received:", event.data);

        const new_event = JSON.parse(event.data);
        var lister = document.getElementById("func_lister");

        // function-local transforms
        if (new_event.type == "OPT") {
            // if we discover a new function, we should initialize and
            // make it visible in the selector.
            var new_fn = false;
            if (!funcs[new_event.name]) {
                funcs[new_event.name] = [];

                // update UI
                var option = document.createElement("option");
                option.text = new_event.name;
                option.value = new_event.name;
                lister.add(option);
                new_fn = true;
            }

            funcs[new_event.name].push(new_event);

            if (new_fn && ++func_cnt == 1) {
                lister.value = new_event.name;
                refresh_func_view(true);
            } else if (new_event.name == lister.value) {
                refresh_pass_list();
            }
        } else if (new_event.type == "LOG") {
            logs += new_event.content + "<br>";

            var log_elem = document.getElementById("log");
            log_elem.innerHTML = logs;
            log_elem.scrollTop = log_elem.scrollHeight;
        }
    };
}

function select_pass(e) {
    console.log("Selected " + e.target.value);

    const new_idx = parseInt(e.target.value);
    if (new_idx == selected_pass) {
        return;
    }
    selected_pass = new_idx;
    var fn = refresh_func_view(false);

    // update selector indicator
    if (old_selected != null) {
        old_selected.className = pass_class_name(fn, old_selected.value, false);
    }
    e.target.className = pass_class_name(fn, new_idx, true);
    old_selected = e.target;
}

function pass_class_name(fn, i, selected) {
    var str = 'pass-button';
    if (i != 0 && fn[i - 1].content != fn[i].content) {
        str += ' pass-changed';
    }

    if (selected) {
        str += ' pass-active';
    }
    return str;
}

function refresh_pass_list() {
    var fn_lister = document.getElementById("func_lister");
    var lister = document.getElementById("pass-list");
    var view = document.getElementById("ir_view");

    var fn = funcs[fn_lister.value];
    lister.innerHTML = '';

    for (var i = 0; i < fn.length; i++) {
        var btn = document.createElement("button");
        btn.className = pass_class_name(fn, i, selected_pass == i);
        if (selected_pass == i) {
            old_selected = btn;
        }
        btn.textContent = fn[i].desc;
        btn.value = i;
        btn.addEventListener('click', select_pass, false);
        lister.appendChild(btn);
    }
    return fn;
}

function refresh_func_view(change) {
    var lister = document.getElementById("func_lister");
    var view = document.getElementById("ir_view");

    if (change) {
        old_selected = null;
        refresh_pass_list();
    }

    var fn = funcs[lister.value];
    if (selected_pass > 0) {
        let span = null;
        const diff = Diff.diffLines(fn[selected_pass - 1].content, fn[selected_pass].content),
        fragment = document.createDocumentFragment();

        diff.forEach((part) => {
                var txt = document.createTextNode(part.value);
                if (!part.added && !part.removed) {
                    fragment.appendChild(txt);
                } else {
                    // green for additions, red for deletions
                    // grey for common parts
                    const color = part.added ? 'ins' : 'del';
                    span = document.createElement(color);
                    // span.style.color = color;
                    span.appendChild(document.createTextNode(part.value));
                    fragment.appendChild(span);
                }
            });

        view.innerHTML = '';
        view.appendChild(fragment);
    } else {
        view.textContent = fn[selected_pass].content;
    }
    return fn;
}

