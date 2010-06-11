$(function() {
    $.plot($('#history-diagram'), [
    {
        data: hu,
        label: 'Tracked packages',
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
            mode: "time"
        },
        yaxis: {
            tickDecimals: 0,
            min: 0
        }
    });
});