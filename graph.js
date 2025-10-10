let players = new Map();


function init() {
    let resp = fetch('boog.json')
        .then((response) => response.json())
        .then(load_players);
}

function load_players(boog_json) {
    let all_names = [];
    for (let player_name in boog_json) {
        all_names.push(player_name);
        let cumulative = [];
        let by_age = [];
        let hof = [];
        let bbwaa = [];
        let seasons = boog_json[player_name];
        for (let season of seasons) {
            cumulative.push([season[0], season[2], player_name]);
            by_age.push([season[0], season[1], player_name]);
            hof.push([season[0], season[3], player_name]);
            bbwaa.push([season[0], season[4], player_name])
        }
        let player = new Map();
        player.set('cumulative', cumulative);
        player.set('age', by_age);
        player.set('hof', hof);
        player.set('bbwaa', bbwaa);
        players.set(player_name, player);
    }

    let collator = new Intl.Collator('en');
    all_names.sort(collator.compare);
    load_datalist_names(all_names);
    setup_events();
}

function load_datalist_names(names) {
    let datalist = document.getElementById('player-list');
    if (!datalist) {
        console.log("Couldn't load data-list element");
        return;
    }

    let options = [];

    let option = document.createElement('option');
    option.value = "";
    option.innerText = "Add players";
    options.push(option);

    for (let name of names) {
        let option = document.createElement('option');
        option.value = name;
        option.innerText = name;
        options.push(option);
    }

    datalist.replaceChildren(...options);
}

function setup_events() {
    let datalist = document.getElementById('player-list');
    if (!datalist) {
        return;
    }
    datalist.addEventListener('change', select_player);

    let graph = document.getElementById('create');
    if (!graph) {
        return;
    }
    graph.addEventListener('click', create_graph);
}

function select_player(ev) {
    let player_name = ev.target.value;
    if (player_name === "" || !players.has(player_name)) {
        return;
    }

    add_player_to_list(player_name);
}

function add_player_to_list(player_name) {
    let player_list = document.getElementById('chosen-players');
    if (!player_list) {
        return;
    }

    // Check if the player is already in the list to avoid duplicates.
    for (let node of player_list.childNodes) {
        if (node.dataset && node.dataset.playerName === player_name) {
            return;
        }
    }

    let player_li = document.createElement('li');
    let name = document.createTextNode(player_name);
    let link_before = document.createTextNode(' (');
    let link_after = document.createTextNode(')');
    let remove_link = document.createElement('a');
    remove_link.innerText = 'remove';
    remove_link.href = '#';
    remove_link.dataset.playerName = player_name;
    remove_link.addEventListener('click', remove_player);
    player_li.appendChild(name);
    player_li.appendChild(link_before);
    player_li.appendChild(remove_link);
    player_li.appendChild(link_after);
    player_li.dataset.playerName = player_name;
    player_list.appendChild(player_li);

    // add 'Remove All' button if not present
    if (!document.getElementById('remove-all')) {
        let players_div = document.getElementById('add-players');
        let remove_all = document.createElement('button');
        remove_all.id = 'remove-all';
        remove_all.innerText = 'Remove All Players';
        remove_all.addEventListener('click', remove_all_players);
        players_div.appendChild(remove_all);
    }
}

function remove_all_players(ev) {
    ev.preventDefault();
    let player_list = document.getElementById('chosen-players');
    if (!player_list) {
        return;
    }

    while (player_list.firstChild) {
        player_list.removeChild(player_list.lastChild);
    }
    let remove_all_btn = document.getElementById('remove-all');
    if (!remove_all_btn) {
        return;
    }
    remove_all_btn.remove();
}

function remove_player(ev) {
    ev.preventDefault();
    let player_list = document.getElementById('chosen-players');
    if (!player_list) {
        return;
    }

    let target = ev.target;
    let target_player = target.dataset.playerName;
    for (let node of player_list.childNodes) {
        if (node.dataset && node.dataset.playerName === target_player) {
            player_list.removeChild(node);
            break;
        }
    }
}

function create_graph(ev) {
    let player_list = document.getElementById('chosen-players');
    if (!player_list || !player_list.hasChildNodes()) {
        return;
    }

    let cumulative = [];
    let by_age = [];
    let hof = [];
    let bbwaa = [];
    for (let node of player_list.childNodes) {
        if (!node.dataset || !node.dataset.playerName) {
            continue;
        }
        const name = node.dataset.playerName;
        let player_data = players.get(name);
        cumulative.push(player_data.get('cumulative'));
        by_age.push(player_data.get('age'));
        hof.push(player_data.get('hof'));
        bbwaa.push(player_data.get('bbwaa'));
    }

    graph_points('cumulative', cumulative, "Cumulative BOOG", "BOOG Score");
    svg_to_image('cumulative');
    graph_points('age', by_age, "BOOG By Age", "BOOG Score");
    svg_to_image('age');
    graph_points('hof', hof, "Hall of Fame Chances By Age", "Probability", true);
    svg_to_image('hof');
    graph_points('bbwaa', bbwaa, "BBWAA Induction Chances By Age", "Probability", true);
    svg_to_image('bbwaa');
}

function graph_points(element_id, seasons, title, y_label_text, y_percentage = false) {
    const graph_dimensions = {
        height: 500,
        width: 800,
        legend: 200,
        margin: 10,
        top: 50,
        right: 10,
        bottom: 50,
        left: 60,
    };

    const legend_dimensions = {
        box: 20,
        box_offset: 30,
    };

    // Clear any existing graphs first.
    d3.select(`#${element_id}`).selectAll('svg').remove();

    let svg = d3.select(`#${element_id}`)
        .append('svg')
        .attr('height', graph_dimensions.height + graph_dimensions.top + graph_dimensions.bottom)
        .attr('width', graph_dimensions.width + graph_dimensions.left + graph_dimensions.right + graph_dimensions.legend);

    // Add the title.
    const graph_title = svg.append('text')
        .attr('x', '50%')
        .attr('y', (graph_dimensions.top + graph_dimensions.margin) / 2)
        .attr('font-size', '150%')
        .style('text-anchor', 'middle')
        .text((d) => title)
        ;

    // Add the labels for the axes.
    const x_label_y = graph_dimensions.top + graph_dimensions.height + graph_dimensions.bottom - graph_dimensions.margin;
    const x_label = svg.append('text')
        .attr('x', '50%')
        .attr('y', x_label_y)
        .style('text-anchor', 'middle')
        .text((d) => "Age (years)")
        ;
    const y_label = svg.append('text')
        .attr('x', graph_dimensions.margin)
        .attr('y', '50%')
        .attr('transform', `rotate(-90 ${graph_dimensions.margin} ${x_label_y / 2 - graph_dimensions.margin})`)
        .style('text-anchor', 'middle')
        .text((d) => y_label_text)
        ;

    // Create the graph.
    let graph = svg
        .append('g')
        .attr('transform', `translate(${graph_dimensions.left}, ${graph_dimensions.top})`)
        ;

    const age_extents = seasons.flatMap((season) => d3.extent(season, d => d[0]));
    const x = d3.scaleLinear()
        .domain(d3.extent(age_extents))
        .range([0, graph_dimensions.width]);
        ;
    graph.append('g')
        .attr('transform', `translate(0, ${graph_dimensions.height})`)
        .call(d3.axisBottom(x));

    const boog_extents = seasons.flatMap((season) => d3.extent(season, d => d[1]));
    const y = d3.scaleLinear()
        .domain(d3.extent(boog_extents))
        .range([graph_dimensions.height, 0])
        .nice();
    let y_axis_formatter = (x) => x;
    // If the y-axis uses percentages then format them to a pretty xx% rather
    // than keeping them in the range of [0, 1].
    if (y_percentage) {
        const y_ticks = y.ticks();
        const max_tick = y_ticks[y_ticks.length - 1] * 100;
        // If the last tick, the largest, is greater than the total number of
        // ticks, then use only integer labels. Otherwise include on decimal
        // digit.
        if (max_tick > y_ticks.length) {
            y_axis_formatter = d3.format(".0%");
        }
        else {
            y_axis_formatter = d3.format(".1%");
        }
    }
    const y_axis = d3.axisLeft(y)
        .tickSize(-graph_dimensions.width)
        .tickFormat(y_axis_formatter);
    graph.append('g')
        .attr('class', 'grid')
        .call(y_axis);

    const color = d3.scaleOrdinal(d3.schemeTableau10);
    const line = d3.line()
        .x((d) => x(d[0]))
        .y((d) => y(d[1]));
    graph.append('g')
        .attr('stroke-width', 2.5)
        .attr('fill', 'none')
        .selectAll('path')
        .data(seasons)
        .join('path')
            .attr('d', line)
            .attr('stroke', color)
        ;

    // Create the legend.
    const names = seasons.flatMap((season) => season[0][2]);
    var legend_area = svg.append('g')
        .attr('transform', `translate(${graph_dimensions.left + legend_dimensions.box + graph_dimensions.width}, ${graph_dimensions.top})`);
    var legend = legend_area.selectAll('.legend')
        .data(names)
        .enter()
            .append('g')
            .attr('class', 'legend')
            .attr('transform', (d, i) => `translate(0, ${i * legend_dimensions.box_offset})`)
        ;

    // TODO: Use same color variable instead of resetting.
    const color_names = d3.scaleOrdinal(d3.schemeTableau10);
    legend.append('rect')
        .attr('x', 0)
        .attr('width', legend_dimensions.box)
        .attr('height', legend_dimensions.box)
        .style('fill', color_names)
        ;

    legend.append('text')
        .attr('x', legend_dimensions.box + 5)
        .attr('y', legend_dimensions.box / 2)
        .attr('dy', '.35em')
        .style('text-anchor', 'start')
        .text((d) => d)
        ;
}

function svg_to_image(element_id) {
    const element = document.getElementById(element_id);
    if (!element) {
        return;
    }
    const canvas_id = element_id + '-canvas';
    const canvas = document.getElementById(canvas_id);
    if (!canvas) {
        return;
    }
    const anchor_id = element_id + '-image';
    const anchor = document.getElementById(anchor_id);
    if (!anchor) {
        return;
    }

    let svg;
    for (let child of element.childNodes) {
        if (child.nodeName === 'svg') {
            svg = child;
            break;
        }
    }

    const svg_xml = (new XMLSerializer()).serializeToString(svg);
    const svg_blob = new Blob([svg_xml], { type: 'image/svg+xml;charset=utf-8' });

    const url = URL.createObjectURL(svg_blob);

    const image = new Image();
    image.width = svg.width.baseVal.value;
    image.height = svg.height.baseVal.value;
    image.src = url;
    image.onload = function () {
        canvas.width = image.width;
        canvas.height = image.height;

        const ctx = canvas.getContext('2d');
        // Draw a white background frst because the SVG is transparent.
        ctx.fillStyle = 'white';
        ctx.fillRect(0, 0, canvas.width, canvas.height);
        ctx.drawImage(image, 0, 0);
        URL.revokeObjectURL(url);
        let data = canvas.toDataURL('image/png');
        anchor.className = "";
        anchor.href = data;
    };
}


init();
