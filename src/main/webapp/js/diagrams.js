$(function() {
    var data = [], i = 0;
    for(category in cd) {
        data.push({
           label: category,
           data: [[i, cd[category]]],
           bars: {
               show: true
           }
        });
        i++;
    }
    $.plot($('#category-diagram'), data, {
        xaxis: {
            ticks: []
        },
        yaxis: {
            tickDecimals: 0,
            min: 0
        }
    });
    $.plot($('#history-diagram'), [
    {
        data: hd,
        lines: {
            show: true,
            fill: true
        },
        points: {
            show: true
        }
    }
    ], {
        xaxis: {
            mode: "time",
            timeformat: "%d %b %y"
        },
        yaxis: {
            tickDecimals: 0,
            min: 0
        }
    });
});
