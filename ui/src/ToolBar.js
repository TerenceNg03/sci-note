import {React} from 'react';
import {PlusOutlined, DeleteOutlined, HeartOutlined, HeartTwoTone} from '@ant-design/icons';
import {Button} from 'antd';

const ToolBar = (isFavorite) => {
    let icon = <HeartOutlined />
    if (isFavorite === true) {
        icon = <HeartTwoTone />
    }
    return (
        <div style={{display: 'inline-flex'}}>
            <Button type={'text'} size={'large'} icon={icon} />
            <Button type={'text'} size={'large'} icon={<DeleteOutlined />} />
            <Button type={'text'} size={'large'} icon={<PlusOutlined />} />
        </div>
    )
}

export default ToolBar;
