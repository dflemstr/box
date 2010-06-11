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
        },
        colors: ['#73C2FF', '#FFD969', '#8080FF', '#FFBF69', '#0A93FC', '#FFBF00', '#1717FC', '#FF9200']
    });
    $.plot($('#history-diagram'), [
    {
        data: hd,
        label: 'Downloads that day',
        color: '#C3D9FF',
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
            timeformat: "%d %b"
        },
        yaxis: {
            tickDecimals: 0,
            min: 0
        }
    });
});
