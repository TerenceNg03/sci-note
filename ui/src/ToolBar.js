import {React} from 'react';
import {PlusOutlined, DeleteOutlined, HeartOutlined, HeartTwoTone} from '@ant-design/icons';
import {Button} from 'antd';

const ToolBar = (props) => {
    let icon = <HeartOutlined />
    if (props.isFavorite === true) {
        icon = <HeartTwoTone />
    }
    return (
        <div id="toolbar" style={{display: 'inline-flex', padding: '0em 2em'}}>
            <Button type={'text'} size={'large'} icon={icon} />
            <Button type={'text'} size={'large'} icon={<DeleteOutlined />} />
        <Button type={'text'} size={'large'} icon={<PlusOutlined />} onClick={
            () => fetch('/api/new', {method: 'POST'})
                    .then((response) => response.json())
                    .then((r) => {
                        props.setPaperId(r.id)
                    })
            } />
        </div>
    )
}

export default ToolBar;
