let players = new Map();


function init() {
    let resp = fetch('maws.json')
        .then((response) => response.json())
        .then(load_players);
}

function load_players(maws_json) {
    let all_names = [];
    for (let player_name in maws_json) {
        all_names.push(player_name);
        let cumulative = [];
        let by_age = [];
        let seasons = maws_json[player_name];
        for (let season of seasons) {
            cumulative.push([season.Age, season.Cumulative, player_name]);
            by_age.push([season.Age, season.MAWS, player_name]);
        }
        let player = new Map();
        player.set('cumulative', cumulative);
        player.set('age', by_age);
        players.set(player_name, player);
    }

    all_names.sort();
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
    let by_age = []
    for (let node of player_list.childNodes) {
        if (!node.dataset || !node.dataset.playerName) {
            continue;
        }
        const name = node.dataset.playerName;
        let player_data = players.get(name);
        cumulative.push(player_data.get('cumulative'));
        by_age.push(player_data.get('age'));
    }

    graph_points('cumulative', cumulative);
    graph_points('age', by_age);
}

function graph_points(element_id, seasons) {
    const graph_dimensions = {
        height: 500,
        width: 800,
        legend: 200,
        top: 10,
        right: 20,
        bottom: 40,
        left: 30,
    };

    const legend_dimensions = {
        box: 20,
        box_offset: 30,
    };

    console.log(seasons);

    // Clear any existing graphs first.
    d3.select(`#${element_id}`).selectAll('svg').remove();

    let svg = d3.select(`#${element_id}`)
        .append('svg')
        .attr('height', graph_dimensions.height + graph_dimensions.top + graph_dimensions.bottom)
        .attr('width', graph_dimensions.width + graph_dimensions.left + graph_dimensions.right + graph_dimensions.legend);

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

    const maws_extents = seasons.flatMap((season) => d3.extent(season, d => d[1]));
    const y = d3.scaleLinear()
        .domain(d3.extent(maws_extents))
        .range([graph_dimensions.height, 0])
        .nice();
    graph.append('g')
        .attr('class', 'grid')
        .call(d3.axisLeft(y)
            .tickSize(-graph_dimensions.width));

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

    const names = seasons.flatMap((season) => season[0][2]);
    console.log(names);
    var legend_area = svg.append('g')
        .attr('transform', `translate(${graph_dimensions.left * 2 + graph_dimensions.width}, 0)`);
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


init();
