import {React} from 'react';
import {Menu} from 'antd';

const menuItems = [
    getItem('Favorite', 'favourite', null, [getItem('Option 13', '0'), getItem('Option 14', '1')], 'group'),
    getItem('Recent', 'recent', null, [getItem('Option 13', '3'), getItem('Option 14', '4')], 'group'),
    getItem('Tags', 'tags', null, [getItem('Option 13', '5'), getItem('Option 14', '6')], 'group'),
];

function getItem(label, key, icon, children, type) {
    return {key, icon, children, label, type};
}

const QuickAccess = () => {
    return (
        <Menu items={menuItems} style={{padding: '1em', height: '100vh', overflow: 'scroll'}} />
    )
}

export default QuickAccess;
