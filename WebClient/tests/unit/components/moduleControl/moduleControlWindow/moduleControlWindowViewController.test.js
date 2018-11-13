'use strict';

import ModuleControlWindowViewController from 
    'components/moduleControl/moduleControlWindow/moduleControlWindowViewController';
import ModelCollection from 'core/modelCollection';
import ModuleModel, { ModuleStatus } from 'components/moduleControl/moduleModel';

describe('ModuleControlWindowViewController', () => {
    let modules;
    let moduleControlWindowViewController;
    let moduleModel;
    
    beforeEach(() => {
        modules = new ModelCollection({
            models: [
                new ModuleModel({
                    id: 'Air2',
                    name: 'Air Quality1',
                    progress: 12345,
                    status: ModuleStatus.IDLE
                }),
                new ModuleModel({
                    id: 'Air2',
                    name: 'Air Quality2',
                    progress: 12345,
                    status: ModuleStatus.IDLE
                }),
                new ModuleModel({
                    id: 'Air3',
                    name: 'Air Quality3',
                    progress: 12345,
                    status: ModuleStatus.IDLE
                })
            ]
        });

        moduleControlWindowViewController = new ModuleControlWindowViewController({ modules });
        moduleControlWindowViewController.showModuleControlWindowView();

        moduleModel = new ModuleModel({
            id: 'Traffic',
            name: 'Traffic',
            progress: 12345,
            status: ModuleStatus.IDLE
        });

        modules.add(moduleModel);
    });

    it('ready list does not contain modules with unexpected status', ()=> {
        moduleModel.status = ModuleStatus.IDLE;
        expect(moduleControlWindowViewController.model().readyModules.length).toBe(0);

        moduleModel.status = ModuleStatus.LOCKED;
        expect(moduleControlWindowViewController.model().readyModules.length).toBe(0);

        moduleModel.status = ModuleStatus.CALCULATING;
        expect(moduleControlWindowViewController.model().readyModules.length).toBe(0);

        moduleModel.status = ModuleStatus.BUSY;
        expect(moduleControlWindowViewController.model().readyModules.length).toBe(0);

        moduleModel.status = ModuleStatus.REMOVED;
        expect(moduleControlWindowViewController.model().readyModules.length).toBe(0);
    });

    it('busy list does not contain modules with unexpected status', ()=> {
        moduleModel.status = ModuleStatus.IDLE;
        expect(moduleControlWindowViewController.model().busyModules.length).toBe(0);

        moduleModel.status = ModuleStatus.LOCKED;
        expect(moduleControlWindowViewController.model().busyModules.length).toBe(0);

        moduleModel.status = ModuleStatus.READY;
        expect(moduleControlWindowViewController.model().busyModules.length).toBe(0);

        moduleModel.status = ModuleStatus.REMOVED;
        expect(moduleControlWindowViewController.model().busyModules.length).toBe(0);
    });
    
    it('ready list contains modules with "ready" status', () => {
        moduleModel.status = ModuleStatus.READY;

        expect(moduleControlWindowViewController.model().readyModules.length).toBe(1);
        expect(moduleControlWindowViewController.model().readyModules.models[0]).toMatchObject({
            id: moduleModel.id,
            name: moduleModel.name,
            status: 'ready',
            loading: false
        });
    });

    it('busy list contains modules with "calculating" status', () => {
        moduleModel.status = ModuleStatus.CALCULATING;

        expect(moduleControlWindowViewController.model().busyModules.length).toBe(1);
        expect(moduleControlWindowViewController.model().busyModules.models[0]).toMatchObject({
            id: moduleModel.id,
            name: moduleModel.name,
            status: 'busy...',
            loading: true
        });
    });

    it('busy list contains modules with "busy" status', () => {
        moduleModel.status = ModuleStatus.BUSY;

        expect(moduleControlWindowViewController.model().busyModules.length).toBe(1);
        expect(moduleControlWindowViewController.model().busyModules.models[0]).toMatchObject({
            id: moduleModel.id,
            name: moduleModel.name,
            status: 'busy...',
            loading: true
        });
    });

    it('module item is transfered from ready list to busy list', () => {
        moduleModel.status = ModuleStatus.READY;
        moduleModel.status = ModuleStatus.BUSY;
        
        expect(moduleControlWindowViewController.model().readyModules.length).toBe(0);
        expect(moduleControlWindowViewController.model().busyModules.length).toBe(1);
        expect(moduleControlWindowViewController.model().busyModules.models[0]).toMatchObject({
            id: moduleModel.id,
            name: moduleModel.name,
            status: 'busy...',
            loading: true
        });
    });

    it('module item is transfered from busy list to ready list', () => {
        moduleModel.status = ModuleStatus.BUSY;
        moduleModel.status = ModuleStatus.READY;
        
        expect(moduleControlWindowViewController.model().busyModules.length).toBe(0);
        expect(moduleControlWindowViewController.model().readyModules.length).toBe(1);        
        expect(moduleControlWindowViewController.model().readyModules.models[0]).toMatchObject({
            id: moduleModel.id,
            name: moduleModel.name,
            status: 'ready',
            loading: false
        });
    });

    it('existing module item instance presists on status changes', () => {
        let actualModuleItemModel;

        moduleModel.status = ModuleStatus.BUSY;
        const expectedModuleItemModel = moduleControlWindowViewController.model().busyModules.models[0];
        
        moduleModel.status = ModuleStatus.IDLE;        
        moduleModel.status = ModuleStatus.CALCULATING;

        actualModuleItemModel = moduleControlWindowViewController.model().busyModules.models[0];
        expect(Object.is(actualModuleItemModel, expectedModuleItemModel)).toBe(true);
        
        moduleModel.status = ModuleStatus.READY;
        
        actualModuleItemModel = moduleControlWindowViewController.model().readyModules.models[0];
        expect(Object.is(actualModuleItemModel, expectedModuleItemModel)).toBe(true);
    });

});
