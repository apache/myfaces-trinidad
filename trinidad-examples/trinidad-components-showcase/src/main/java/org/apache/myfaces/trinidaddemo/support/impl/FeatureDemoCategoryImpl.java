package org.apache.myfaces.trinidaddemo.support.impl;

import org.apache.myfaces.trinidaddemo.support.IFeatureDemoCategory;
import org.apache.myfaces.trinidaddemo.support.FeatureDemoCategoryId;
import org.apache.myfaces.trinidaddemo.support.IFeatureDemo;

import java.util.List;
import java.util.ArrayList;

/**
 * Default implementation of the {@link IFeatureDemoCategory} interface.
 */
public class FeatureDemoCategoryImpl implements IFeatureDemoCategory {

    private static final long serialVersionUID = -7078447662522060454L;

    private FeatureDemoCategoryId id;
	private String name;

	private List<IFeatureDemo> featureDemos;

    /**
	 * Constructor.
	 *
	 * @param id the unique id of this category.
	 * @param name the name of this category.
	 */
	public FeatureDemoCategoryImpl(FeatureDemoCategoryId id, String name) {
		this.id = id;
		this.name = name;

		featureDemos = new ArrayList<IFeatureDemo>();
	}

	public void addFeatureDemo(IFeatureDemo featureDemo) {
		featureDemo.setCategory(this);
		featureDemos.add(featureDemo);
	}

	public List<IFeatureDemo> getFeatureDemos() {
		return featureDemos;
	}

	public FeatureDemoCategoryId getId() {
		return id;
	}

	public String getName() {
		return name;
	}

    @Override
	public boolean equals(Object obj) {
		if (obj instanceof FeatureDemoCategoryImpl) {
			return getId() == ((FeatureDemoCategoryImpl)obj).getId();
		}

		return false;
	}

	@Override
	public int hashCode() {
		return getId() != null ? getId().hashCode() : super.hashCode();
	}

	@Override
	public String toString() {
		return getName();
	}
}
